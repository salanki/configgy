package net.lag.configgy
package configvalue

import scala.collection.immutable.TreeMap
import net.lag.extensions._

/**
 * Complex configuration types
 */
sealed trait ConfigValue {
  def toConfigStringList: List[String]
}

object ConfigObject {
  def apply[A <: ConfigValue](map: Map[String, A]) = new ConfigObject(TreeMap[String, ConfigValue]()(CaseInsensitiveOrdering) ++ map)
  def apply[A <: ConfigValue](map: (String, A)*) = new ConfigObject(TreeMap[String, ConfigValue]()(CaseInsensitiveOrdering) ++ map)
  def apply() = new ConfigObject(TreeMap[String, ConfigValue]()(CaseInsensitiveOrdering))

}
case class ConfigObject[+A <: ConfigValue](entries: TreeMap[String, A]) extends ConfigValue {
  override def toString = "{" + entries.map({ case (k, v) => k + "=" + v }).mkString(" ") + "}"
  def toConfigStringList = entries match {
    // case x if x.size < 4 => ("{" + x.map({case (k, v) => s"$k = "+v.toConfigStringList}).mkString(" ") + "}") :: Nil // ??
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
case class ConfigList[+A <: ConfigValue](entries: List[A]) extends ConfigValue {
  override def toString = entries.mkString("[", ",", "]")

  def toConfigStringList = {
    val subLists = entries.map(_.toConfigStringList)
    entries match {
      case x if x.size < 4 && subLists.find(_.size > 1) == None =>
        ("[" + subLists.map(_.head).mkString(", ") + "]") :: Nil // ??
      case x =>
        val out = for { 
          a <- x
          ls = a.toConfigStringList
          b <- ls.updated(ls.length-1, ls.last+",")
        } yield s"  $b"
        "[" ::
          out.updated(out.length-1, out.last.init) :::
          "]" :: Nil
    }
  }
}

case class ConfigString(value: String) extends ConfigValue {
  override def toString = "\"" + value + "\""
  def toConfigStringList = "\"" + value.quoteC + "\"" :: Nil
}
case class ConfigInt(value: Long) extends ConfigValue {
  override def toString = value.toString
  def toConfigStringList = value.toString :: Nil
}
case class ConfigDouble(value: Double) extends ConfigValue {
  override def toString = value.toString
  def toConfigStringList = value.toString :: Nil

}
case class ConfigBoolean(value: Boolean) extends ConfigValue {
  override def toString = value.toString
  def toConfigStringList = value.toString :: Nil

}
