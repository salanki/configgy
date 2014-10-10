package net.lag.configgy
package configvalue

import scala.collection.immutable.{ TreeMap, MapProxy }
import net.lag.extensions._

/**
 * Complex configuration types
 */
sealed trait ConfigValue {
  def toConfigStringList: List[String]
}

object ConfigObject {
  def apply[A <: ConfigValue](map: Map[String, A]) = new ConfigObject(TreeMap[String, A]()(CaseInsensitiveOrdering) ++ map)
  def apply[A <: ConfigValue](map: (String, A)*) = new ConfigObject(TreeMap[String, A]()(CaseInsensitiveOrdering) ++ map)
  def apply() = new ConfigObject(TreeMap[String, ConfigValue]()(CaseInsensitiveOrdering))

}
case class ConfigObject[+A <: ConfigValue](entries: TreeMap[String, A]) extends ConfigValue with MapProxy[String, A] {
  override def toString = "{" + entries.map({ case (k, v) => k + "=" + v }).mkString(" ") + "}"

  def self = entries

  def toConfigStringList = entries match {
    case x =>
      "{" ::
        x.toList.flatMap({
          case (k, v) =>
            val head :: tail = v.toConfigStringList
            s"  $k = $head" :: tail.map("  " + _)
        }) :::
        "}" :: Nil
  }
}

object ConfigList {
  def apply[A <: ConfigValue](entries: A*): ConfigList[A] = ConfigList(entries.toList)
}
case class ConfigList[+A <: ConfigValue](entries: List[A]) extends ConfigValue with Equals {
  override def toString = entries.mkString("[", ",", "]")

  /**
   * A special equals that also matches a list with a single element to the same element without a list encapsulation.
   */
  override def equals(other: Any) = other match {
    case that: ConfigList[A] => this.entries == that.entries
    case that if entries.length == 1 => entries.head == that
    case _ => false
  }

  def toConfigStringList = {
    val subLists = entries.map(_.toConfigStringList)
    entries match {
      case x if x.size < 4 && subLists.find(_.size > 1) == None =>
        ("[" + subLists.map(_.head).mkString(", ") + "]") :: Nil // ??
      case x =>
        val out = for {
          a <- x
          ls = a.toConfigStringList
          b <- ls.updated(ls.length - 1, ls.last + ",")
        } yield s"  $b"
        "[" ::
          out.updated(out.length - 1, out.last.init) :::
          "]" :: Nil
    }
  }
}

object ConfigString {
    implicit val ordering: Ordering[ConfigString] = Ordering.by(_.value)
}

case class ConfigString(value: String) extends ConfigValue {
  /**
   * A special equals that also matches a list with a single element to the same element without a list encapsulation.
   */
  override def equals(other: Any) = other match {
    case that: ConfigString => this.value == that.value
    case that: ConfigList[_] if that.entries.length == 1 => that.entries.head == this
    case _ => false
  }

  override def toString = "\"" + value + "\""
  def toConfigStringList = "\"" + value.quoteC + "\"" :: Nil
}

case class ConfigInt(value: Long) extends ConfigValue {
  /**
   * A special equals that also matches a list with a single element to the same element without a list encapsulation.
   */
  override def equals(other: Any) = other match {
    case that: ConfigInt => this.value == that.value
    case that: ConfigList[_] if that.entries.length == 1 => that.entries.head == this
    case _ => false
  }

  override def toString = value.toString
  def toConfigStringList = value.toString :: Nil
}
case class ConfigDouble(value: Double) extends ConfigValue {
  /**
   * A special equals that also matches a list with a single element to the same element without a list encapsulation.
   */
  override def equals(other: Any) = other match {
    case that: ConfigDouble => this.value == that.value
    case that: ConfigList[_] if that.entries.length == 1 => that.entries.head == this
    case _ => false
  }

  override def toString = value.toString
  def toConfigStringList = value.toString :: Nil

}
case class ConfigBoolean(value: Boolean) extends ConfigValue {
  /**
   * A special equals that also matches a list with a single element to the same element without a list encapsulation.
   */
  override def equals(other: Any) = other match {
    case that: ConfigBoolean => this.value == that.value
    case that: ConfigList[_] if that.entries.length == 1 => that.entries.head == this
    case _ => false
  }

  override def toString = value.toString
  def toConfigStringList = value.toString :: Nil

}
