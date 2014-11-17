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

import scala.annotation._, elidable._
import scala.collection.mutable.Stack
import scala.collection.immutable.TreeMap
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import net.lag.extensions._
import configvalue._

private case object ObjectOpen

/**
 * An exception thrown when parsing a config file, if there was an error
 * during parsing. The `reason` string will contain the parsing
 * error details.
 */
class ParseException(reason: String, cause: Throwable) extends Exception(reason, cause) {
  def this(reason: String) = this(reason, null)
  def this(cause: Throwable) = this(null, cause)
}

private[configgy] class ConfigParser(var attr: Attributes, val importer: Importer, val sections: Stack[String] = new Stack[String], var prefix: String = "") extends RegexParsers {
  // Stack reversed iteration order from 2.7 to 2.8!!
  def sectionsString = sections.toList.reverse.mkString(".")

  type ObjectStack = List[(String, ConfigObject[ConfigValue])]
  type ObjectList = (String, List[ConfigObject[ConfigValue]])

  var objectStack: ObjectStack = Nil
  var objectStackStash: List[ObjectStack] = Nil

  var objectList: Option[ObjectList] = None
  var objectListStash: List[ObjectList] = Nil

  var aliasStack: List[String] = Nil

   @elidable(FINE) def debug(x: String) = println(x)

  // tokens
  override val whiteSpace = """(\s+|#[^\n]*\n)+""".r
  val numberToken: Parser[String] = """-?\d+(\.\d+)?""".r
  val stringToken: Parser[String] = ("\"" + """([^\\\"]|\\[^ux]|\\\n|\\u[0-9a-fA-F]{4}|\\x[0-9a-fA-F]{2})*""" + "\"").r
  val identToken: Parser[String] = """([\da-zA-Z_][-\w]*)(\.[a-zA-Z_][-\w]*)*""".r
  val assignToken: Parser[String] = """=|\?=""".r
  val tagNameToken: Parser[String] = """[0-9a-zA-Z][-\w]*""".r

  def root = rep(includeFile | includeOptFile | assignment | toggle | sectionOpen | sectionClose |
    sectionOpenBrace | sectionCloseBrace | listClose | listComma | aliasOpenParen | aliasCloseParen)

  def includeFile = "include" ~> string ^^ {
    case filename: String =>
      new ConfigParser(attr, importer, sections, prefix) parse importer.importFile(filename)

  }

  def includeOptFile = "include?" ~> string ^^ {
    case filename: String =>
      new ConfigParser(attr.makeAttributes(sections.mkString(".")), importer) parse importer.importFile(filename, false)
  }

  def assignment = identToken ~ assignToken ~ value ^^ {
    case k ~ a ~ v =>
      objectStack match {
        case Nil => regularAssign(k, a, v)
        case other => objectAssign(k, v)
      }
  }

  def listComma: Parser[Unit] = "," ~ objectOpenBrace ^^ { x =>
    debug("ListComma")
    objectStack = ("", ConfigObject()) :: objectStack
  }

  def listClose: Parser[Unit] = "]" ^^ { x =>
    debug("List close: " + objectList)

    objectStackStash match {
      case objHead :: objTail =>
        objectStack = objHead
        objectStackStash = objTail

        for ((name, list) <- objectList) updateObject(name, ConfigList(list.reverse))
      case Nil =>
        for ((name, list) <- objectList) attr(aliased(name)) = ConfigList(list.reverse)

    }

    objectListStash match {
      case head :: tail =>
        debug("Close inside of another list")

        objectList = Some(head)
        objectListStash = tail

      case Nil =>
        debug("Close outside of another list")
        objectList = None

    }
  }

  def updateObject(k: String, v: ConfigValue) = objectStack match {
    case (name, head) :: tail => objectStack = (name, ConfigObject(head.entries + Pair(k, v))) :: tail
    case Nil => println("Object stack Nil in update. This should never happen")
  }

  def objectAssign(k: String, v: Any) = v match {
    case ObjectOpen => objectStack = (k, ConfigObject()) :: objectStack
    case other =>
      val mapper = primitiveMapper orElse specialMapper
      updateObject(k, mapper(other))
  }

  private val specialMapper: PartialFunction[Any, ConfigValue] = {
    case x: ConfigList[_] => x
  }

  private val primitiveMapper: PartialFunction[Any, ConfigValue] =
    _ match {
      case x: Int => x.toConfig
      case x: Long => x.toConfig
      case x: Double => x.toConfig
      case x: String => x.toConfig
      case x: Boolean => x.toConfig
    }

  def regularAssign(k: String, a: String, v: Any) = if (a match {
    case "=" => true
    case "?=" => !attr.contains(prefix + k)
  }) v match {
    case x: Int => attr(prefix + aliased(k)) = x
    case x: Long => attr(prefix + aliased(k)) = x
    case x: String => attr(prefix + aliased(k)) = x
    case x: Array[_] => attr(prefix + aliased(k)) = x.map(_.toString)
    case x: Boolean => attr(prefix + aliased(k)) = x
    case x: ConfigList[_] => attr(prefix + aliased(k)) = x
    case ObjectOpen => objectStack = (prefix + aliased(k), ConfigObject()) :: Nil
  }

  def toggle = identToken ~ trueFalse ^^ { case k ~ v => attr(prefix + aliased(k)) = v }

  def sectionOpen = "<" ~> tagNameToken ~ rep(tagAttribute) <~ ">" ^^ {
    case name ~ attrList => openBlock(name, attrList)
  }
  def tagAttribute = opt(whiteSpace) ~> (tagNameToken <~ "=") ~ string ^^ { case k ~ v => (k, v) }
  def sectionClose = "</" ~> tagNameToken <~ ">" ^^ { name => closeBlock(Some(name)) }

  def sectionOpenBrace = tagNameToken ~ opt("(" ~> rep(tagAttribute) <~ ")") <~ "{" ^^ {
    case name ~ attrListOption => openBlock(name, attrListOption.getOrElse(Nil))
  }

  def aliasOpenParen = tagNameToken <~ "(" ^^ {
    case name => openAlias(name)
  }

  def aliasCloseParen = ")" ^^ {
    case name => closeAlias()
  }

  def sectionCloseBrace = "}" ^^ { x =>
    if (objectStack != Nil)
      closeObject()
    else
      closeBlock(None)
  }

  private def getConfigMapRecursed(v: String, s: List[String]): Option[Attributes] =
    s match {
      case Nil => None
      case head :: tail =>
        val parent = attr.makeAttributes(tail.reverse.mkString("."))
        parent.getConfigMap(v) match {
          case None => getConfigMapRecursed(v, tail)
          case x: Some[_] => Some(parent.makeAttributes(v))
        }
    }

  private def openBlock(name: String, attrList: List[(String, String)]) = {
    val parent = if (sections.size > 0) attr.makeAttributes(sectionsString) else attr
    sections push name
    prefix = sectionsString + "."
    val newBlock = attr.makeAttributes(sectionsString)

    for ((k, v) <- attrList) k match {
      case "inherit" =>
        newBlock.inherits = (for (x <- v.split(',')) yield getConfigMapRecursed(x, sections.toList) match {
          case Some(a) => a
          case None => attr.makeAttributes(v)
        }).toList
      case _ =>
        throw new ParseException("Unknown block modifier")
    }
  }

  private def closeBlock(name: Option[String]) = {
    if (sections.isEmpty) {
      throw new ParseException("Dangling close tag (}), make sure all your braces match up")
    } else {
      val last = sections.pop
      if (name.isDefined && last != name.get) {
        println("got closing tag for " + name.get + ", expected " + last)
      } else {
        prefix = if (sections.isEmpty) "" else sectionsString + "."
      }
    }
  }

  private def openObject() = {
    ObjectOpen
  }

  private def closeObject() = objectStack match {
    case (name, head) :: tail =>
      tail match {
        case Nil =>
          objectList match {
            case None =>
              /* Last object in stack, time to assign to attribute */
              debug(s"Setting: $name = " + head)
              attr(aliased(name)) = head
            case Some((_, Nil)) =>
              debug("Adding to list first: " + head)
              objectList = Some(name, head :: Nil)
            case Some((n2, lst)) =>
              debug("Adding to later first: " + head)
              objectList = Some(n2, head :: lst)
          }

          objectStack = Nil
        case _ =>
          objectStack = tail
          updateObject(name, head)
      }
    case Nil =>
      println("Object close without stack, this should never happen")
  }

  def aliased(name: String) = (name :: aliasStack).reverse.mkString("-")

  def openAlias(name: String) = {
    attr.config.aliases ::= name
    aliasStack ::= name
  }

  def closeAlias() = aliasStack match {
    case Nil => println("Dangling alias close parenthesis")
    case head :: tail => aliasStack = tail
  }

  def value: Parser[Any] = number | string | objectListOpenBrace | stringList | trueFalse | objectOpenBrace
  def number = numberToken ^^ { x =>
    if (x.contains('.'))
      x
    else
      x match {
        case IntExtractor(i) => i
        case LongExtractor(i) => i
        case _ => throw new ParseException("Number is not Integer or Long")
      }
  }
  def string = stringToken ^^ { s => attr.interpolate(prefix, s.substring(1, s.length - 1).unquoteC) }
  def stringList = "[" ~> repsep(string | number | trueFalse, opt(",")) <~ (opt(",") ~ "]") ^^ { list => ConfigList(list.map(primitiveMapper)) }
  def trueFalse: Parser[Boolean] = ("(true|on)".r ^^ { x => true }) | ("(false|off)".r ^^ { x => false })
  def objectListOpenBrace: Parser[ObjectOpen.type] = "[" ~> opt(whiteSpace) ~> "{" ^^ { x =>
    debug(s"Object List Open Brace: $objectList on objStack: $objectStack")

    objectList foreach (objectListStash ::= _)
    objectList = Some("", Nil)

    if (objectStack != Nil) {
      objectStackStash ::= objectStack
      objectStack = Nil
    }

    openObject()
  }
  def objectOpenBrace: Parser[ObjectOpen.type] = "{" ^^ { x =>
    openObject()
  }

  def parse(in: String): Unit = {
    parseAll(root, in) match {
      case Success(result, _) => 
      case x @ Failure(msg, z) => throw new ParseException(x.toString)
      case x @ Error(msg, _) => throw new ParseException(x.toString)
    }
  }
}
