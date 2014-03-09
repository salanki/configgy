package net.lag.configgy
package configvalue

import scala.collection.immutable.TreeMap

/**
 * Complex configuration types
 */
sealed trait ConfigValue

object ConfigObject {
  def apply[A <: ConfigValue](map: Map[String, A]) = new ConfigObject(TreeMap[String, ConfigValue]()(CaseInsensitiveOrdering) ++ map)
  def apply[A <: ConfigValue](map: (String,A)*) = new ConfigObject(TreeMap[String, ConfigValue]()(CaseInsensitiveOrdering) ++ map)
  def apply() = new ConfigObject(TreeMap[String, ConfigValue]()(CaseInsensitiveOrdering))

}
case class ConfigObject[+A <: ConfigValue](entries: TreeMap[String, A]) extends ConfigValue

object ConfigList {
  def apply[A <: ConfigValue](entries: A*): ConfigList[A] = ConfigList(entries.toList)
}
case class ConfigList[+A <: ConfigValue](entries: List[A]) extends ConfigValue

case class ConfigString(value: String) extends ConfigValue
case class ConfigInt(value: Long) extends ConfigValue
case class ConfigDouble(value: Double) extends ConfigValue
case class ConfigBoolean(value: Boolean) extends ConfigValue
