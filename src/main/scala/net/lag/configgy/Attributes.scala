/*
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.configgy

import java.util.regex.Pattern
import scala.collection.immutable.TreeMap
import scala.collection.{ immutable, mutable, Map }
import scala.util.Sorting
import de.congrace.exp4j.ExpressionBuilder
import net.lag.extensions._

import configvalue._

private[configgy] sealed abstract class Cell
private[configgy] case class StringCell(value: String) extends Cell
private[configgy] case class AttributesCell(attr: Attributes) extends Cell
private[configgy] case class StringListCell(array: List[String]) extends Cell
private[configgy] case class ConfigValueCell(array: ConfigValue) extends Cell

/**
 * Actual implementation of ConfigMap.
 * Stores items in Cell objects, and handles interpolation and key recursion.
 */
private[configgy] class Attributes(val config: Config, val name: String) extends ConfigMap {
  private var cells = TreeMap[String, Cell]()(CaseInsensitiveOrdering)
  private var monitored = false
  var inherits: List[Attributes] = Nil

  def this(config: Config, name: String, copyFrom: ConfigMap) = {
    this(config, name)
    copyFrom.copyInto(this)
  }

  def keys: Iterator[String] = cells.keysIterator

  def getName() = name

  def keyToString(key: String): List[String] =
    cells(key) match {
      case StringCell(x) => stringOrIntToString(x) :: Nil
      case AttributesCell(x) => x.toString :: Nil
      case StringListCell(x) => x.mkString("[", ",", "]") :: Nil
      case ConfigValueCell(x) => x.toString :: Nil
    }

  def keyAliaser(keys: List[List[String]], stack: List[String] = Nil, assignOperator: String = "=", indent: String = "", keyToStringFn: String => List[String] = keyToString): Iterable[String] = {
    def processNode(head: String, lst: List[List[String]]): List[String] =
      if (config.aliases.find(_.toLowerCase == head.toLowerCase) != None) {
        val mappedList = for ((head :: tail) <- lst) yield tail

        s"$head (" ::
          keyAliaser(mappedList, head :: stack, assignOperator, indent, keyToStringFn).toList.map(indent + _) :::
          ")" :: Nil
      } else {
        for {
          ls <- lst
          head :: tail = keyToStringFn((stack.reverse ::: ls).mkString("-"))
          x <- (ls.mkString("-") + assignOperator + head) :: tail
        } yield x
      }
    val grouped = keys.groupBy(_.head)

    for {
      (group, lst) <- grouped
      x <- processNode(group, lst)
    } yield x
  }

  override def toString() = {
    val buffer = new StringBuilder("{")
    buffer ++= name
    buffer ++= (inherits match {
      case Nil => ""
      case a: List[_] => " (inherit=" + a.map(_.name).mkString(",") + ")"
    })
    buffer ++= ": "
    for (row <- keyAliaser(sortedKeys.map(_.split('-').toList).toList)) {
      buffer ++= row
      buffer ++= " "
    }
    buffer ++= "}"
    buffer.toString
  }

  override def equals(obj: Any) = {
    if (!obj.isInstanceOf[Attributes]) {
      false
    } else {
      val other = obj.asInstanceOf[Attributes]
      (other.sortedKeys.toList == sortedKeys.toList) &&
        (cells.keys forall (k => { cells(k) == other.cells(k) }))
    }
  }

  /**
   * Look up a value cell for a given key. If the key is compound (ie,
   * "abc.xyz"), look up the first segment, and if it refers to an inner
   * Attributes object, recursively look up that cell. If it's not an
   * Attributes or it doesn't exist, return None. For a non-compound key,
   * return the cell if it exists, or None if it doesn't.
   */
  private def lookupCell(key: String): Option[Cell] = {
    val elems = key.split("\\.", 2)
    if (elems.length > 1) {
      cells.get(elems(0)) match {
        case Some(AttributesCell(x)) => x.lookupCell(elems(1))
        case None => inherits.find(_.lookupCell(key) isDefined).map(_.lookupCell(key).get)
        case _ => None
      }
    } else {
      cells.get(elems(0)) match {
        case x @ Some(_) => x
        case None => inherits.find(_.lookupCell(key) isDefined).map(_.lookupCell(key).get)
      }
    }
  }

  /**
   * Determine if a key is compound (and requires recursion), and if so,
   * return the nested Attributes block and simple key that can be used to
   * make a recursive call. If the key is simple, return None.
   *
   * If the key is compound, but nested Attributes objects don't exist
   * that match the key, an attempt will be made to create the nested
   * Attributes objects. If one of the key segments already refers to an
   * attribute that isn't a nested Attribute object, a ConfigException
   * will be thrown.
   *
   * For example, for the key "a.b.c", the Attributes object for "a.b"
   * and the key "c" will be returned, creating the "a.b" Attributes object
   * if necessary. If "a" or "a.b" exists but isn't a nested Attributes
   * object, then an ConfigException will be thrown.
   */
  @throws(classOf[ConfigException])
  private def recurse(key: String): Option[(Attributes, String)] = {
    val elems = key.split("\\.", 2)
    if (elems.length > 1) {
      val attr = (cells.get(elems(0)) match {
        case Some(AttributesCell(x)) => x
        case Some(_) => throw new ConfigException("Illegal key " + key)
        case None => createNested(elems(0))
      })
      attr.recurse(elems(1)) match {
        case ret @ Some((a, b)) => ret
        case None => Some((attr, elems(1)))
      }
    } else {
      None
    }
  }

  def replaceWith(newAttributes: Attributes): Unit = {
    // stash away subnodes and reinsert them.
    val subnodes = for ((key, cell @ AttributesCell(_)) <- cells.toList) yield (key, cell)
    cells = cells.empty
    cells ++= newAttributes.cells
    for ((key, cell) <- subnodes) {
      newAttributes.cells.get(key) match {
        case Some(AttributesCell(newattr)) =>
          cell.attr.replaceWith(newattr)
          cells = cells + Pair(key, cell)
        case None =>
          throw new ConfigException("Config: Can't find variable substitution: " + key) /* Need to log or throw exception */
          cell.attr.replaceWith(new Attributes(config, ""))
        case Some(_) => throw new ConfigException("Cell that is not AttributesCell in replaceWith")
      }
    }
  }

  private def createNested(key: String): Attributes = {
    val attr = new Attributes(config, if (name.equals("")) key else (name + "." + key))

    if (monitored) {
      attr.setMonitored
    }
    cells += (key -> new AttributesCell(attr))
    attr
  }

  def getConfigValue(key: String): Option[ConfigValue] = {
    lookupCell(key) flatMap (_ match {
      case StringListCell(x) => Some(x.toList.toConfig)
      case a: StringCell => Some(a.toConfig)
      case ConfigValueCell(x) => Some(x)
      case _ => None
    })
  }

  def getString(key: String): Option[String] = {
    lookupCell(key) match {
      case Some(ConfigValueCell(ConfigString(x))) => Some(x)
      case Some(StringCell(x)) => Some(x)
      case Some(StringListCell(x)) => Some(x.toList.mkString("[", ",", "]"))
      case _ => None
    }
  }

  def getConfigMap(key: String): Option[ConfigMap] = {
    lookupCell(key) match {
      case Some(AttributesCell(x)) => Some(x)
      case _ => None
    }
  }

  def configMap(key: String): ConfigMap = makeAttributes(key, true)

  private[configgy] def makeAttributes(key: String): Attributes = makeAttributes(key, false)

  private[configgy] def makeAttributes(key: String, withInherit: Boolean): Attributes = {
    if (key == "") {
      return this
    }
    recurse(key) match {
      case Some((attr, name)) =>
        attr.makeAttributes(name, withInherit)
      case None =>
        val cell = if (withInherit) lookupCell(key) else cells.get(key)
        cell match {
          case Some(AttributesCell(x)) => x
          case Some(_) => throw new ConfigException("Illegal key " + key)
          case None => createNested(key)
        }
    }
  }

  def getList(key: String): Seq[String] = {
    lookupCell(key) match {
      case Some(ConfigValueCell(ConfigList(a @ List(ConfigString(x), _*)))) => a.asInstanceOf[List[ConfigString]].map(_.value)
      case Some(StringListCell(x)) => x
      case Some(StringCell(x)) => Array[String](x)
      case _ => Array[String]()
    }
  }

  def setString(key: String, value: String): Unit = {
    if (monitored) {
      config.deepSet(name, key, value)
      return
    }

    recurse(key) match {
      case Some((attr, name)) => attr.setString(name, value)
      case None => cells.get(key) match {
        case Some(AttributesCell(_)) => throw new ConfigException("Illegal key " + key)
        case _ => cells += (key -> new StringCell(value))
      }
    }
  }

  def setConfigValue(key: String, value: ConfigValue): Unit = {
    if (monitored) {
      config.deepSet(name, key, value)
      return
    }

    recurse(key) match {
      case Some((attr, name)) => attr.setConfigValue(name, value)
      case None => cells.get(key) match {
        case Some(AttributesCell(_)) => throw new ConfigException("Illegal key " + key)
        case _ => cells += (key -> new ConfigValueCell(value))
      }
    }
  }

  def setList(key: String, value: Seq[String]): Unit = {
    if (monitored) {
      config.deepSet(name, key, value)
      return
    }

    recurse(key) match {
      case Some((attr, name)) => attr.setList(name, value)
      case None => cells.get(key) match {
        case Some(AttributesCell(_)) => throw new ConfigException("Illegal key " + key)
        case _ => cells += (key -> new StringListCell(value.toList))
      }
    }
  }

  def setConfigMap(key: String, value: ConfigMap): Unit = {
    if (monitored) {
      config.deepSet(name, key, value)
      return
    }

    recurse(key) match {
      case Some((attr, name)) => attr.setConfigMap(name, value)
      case None =>
        val subName = if (name == "") key else (name + "." + key)
        cells.get(key) match {
          case Some(AttributesCell(_)) =>
            cells += (key -> new AttributesCell(new Attributes(config, subName, value)))
          case None =>
            cells += (key -> new AttributesCell(new Attributes(config, subName, value)))
          case _ =>
            throw new ConfigException("Illegal key " + key)
        }
    }
  }

  def contains(key: String): Boolean = {
    recurse(key) match {
      case Some((attr, name)) => attr.contains(name)
      case None => cells.contains(key)
    }
  }

  def remove(key: String): Boolean = {
    if (monitored) {
      return config.deepRemove(name, key)
    }

    recurse(key) match {
      case Some((attr, name)) => attr.remove(name)
      case None => {
        cells.get(key) match {
          case Some(_) =>
            cells -= key
            true
          case None => false
        }
      }
    }
  }

  def asMap: Map[String, ConfigValue] = {
    var ret = immutable.Map.empty[String, ConfigValue]
    for ((key, value) <- cells) {
      value match {
        case x : StringCell => ret = ret.updated(key, x.toConfig)
        case StringListCell(x) => ret = ret.updated(key, x.toList.toConfig)
        case ConfigValueCell(x) => ret = ret.updated(key, x)
        case AttributesCell(x) =>
          for ((k, v) <- x.asMap) {
            ret = ret.updated(key + "." + k, v)
          }
      }
    }
    ret
  }

  def keyToStringList(key: String): List[String] = cells(key) match {
    case StringCell(x) => stringOrIntToString(x) :: Nil 
    case StringListCell(x) if x.size < 4 => ("[" + x.map(stringOrIntToString).mkString(", ") + "]") :: Nil // ??
    case StringListCell(x) =>
      "[" ::
        x.toList.map { "  \"" + _.quoteC + "\"," } :::
        "]" :: Nil
    case ConfigValueCell(node) => "Blopp" :: Nil /* TODO: Typeclass for converting ConfigValue to text here */
  }

  def toConfigString: String = {
    toConfigList().mkString("", "\n", "\n")
  }

  private def toConfigList(): List[String] = {
    val buffer = new mutable.ListBuffer[String]

    val remainingKeys =
      for (key <- sortedKeys) yield {
        cells(key) match {
          case AttributesCell(node) =>
            buffer += (key + (inherits match {
              case Nil => ""
              case a: List[_] => " (inherit=" + a.map(_.name).mkString(",") + ")"
            }) + " {")
            buffer ++= node.toConfigList().map { "  " + _ }
            buffer += "}"
            buffer += ""
            None
          case other =>
            Some(key.split('-').toList)
        }
      }

    buffer ++= keyAliaser(remainingKeys.flatten.toList, assignOperator = " = ", indent = " ", keyToStringFn = keyToStringList)

    buffer.toList
  }
  
  private def stringOrIntToString(s: String) = s match {
    case LongExtractor(x) => x.toString
    case x => "\""+x.quoteC+"\""
  }

  def subscribe(subscriber: Subscriber) = {
    config.subscribe(name, subscriber)
  }

  // substitute "$(...)" strings with looked-up vars
  // (and find "\$" and replace them with "$")
  private val INTERPOLATE_RE = """(?<!\\)\$\((\w[\w\d\._-]*)\)|\\\$""".r

  private def getStringRecursed(v: String, s: List[String], cm: ConfigMap): Option[String] =
    s match {
      case Nil => None
      case head :: tail =>
        val str = tail.reverse.mkString(".") match {
          case "" => v
          case str if str.endsWith(".") => str + v
          case other => other + "." + v
        }

        cm.getString(str) match {
          case None => getStringRecursed(v, tail, cm)
          case x: Some[_] => x
        }
    }

  protected[configgy] def interpolate(root: Attributes, s: String, section: String): String = {
    def lookup(key: String, path: List[ConfigMap]): String = {

      path match {
        case Nil => ""
        case attr :: xs => getStringRecursed(key, "" :: section.split('.').toList.reverse, attr) match {
          case Some(x) => x
          case None => lookup(key, xs)
        }
      }
    }

    val replaced = s.regexSub(INTERPOLATE_RE) { m =>
      if (m.matched == "\\$") {
        "$"
      } else {
        lookup(m.group(1), List(this, root, EnvironmentAttributes))
      }
    }

    if (s != replaced) {
      try {
        new ExpressionBuilder(replaced).build().calculate().toLong.toString
      } catch {
        case e: Exception => replaced
      }
    } else replaced
  }

  protected[configgy] def interpolate(key: String, s: String): String = {
    recurse(key) match {
      case Some((attr, name)) => attr.interpolate(this, s, key)
      case None => interpolate(this, s, key)
    }
  }

  /* set this node as part of a monitored config tree. once this is set,
   * all modification requests go through the root Config, so validation
   * will happen.
   */
  protected[configgy] def setMonitored: Unit = {
    if (monitored) {
      return
    }

    monitored = true
    for (cell <- cells.values) {
      cell match {
        case AttributesCell(x) => x.setMonitored
        case _ => // pass
      }
    }
  }

  protected[configgy] def isMonitored = monitored

  // make a deep copy of the Attributes tree.
  def copy(): Attributes = {
    copyInto(new Attributes(config, name))
  }

  def copyInto[T <: ConfigMap](attr: T): T = {
    inherits.foreach(_.copyInto(attr))

    for ((key, value) <- cells.iterator) {
      value match {
        case StringCell(x) => attr(key) = x
        case StringListCell(x) => attr(key) = x
        case ConfigValueCell(x) => attr(key) = x
        case AttributesCell(x) => attr.setConfigMap(key, x.copy())
      }
    }
    attr
  }
}
