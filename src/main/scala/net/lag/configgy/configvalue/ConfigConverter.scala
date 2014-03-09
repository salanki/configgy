package net.lag.configgy
package configvalue

import scala.collection.immutable.TreeMap
import net.lag.extensions._


/**
 * Convert from standard types to ConfigValue wrapped ones
 */
trait ConfigConverter[A] {
  def convertToConfig(value: A): ConfigValue
}

object ConfigConverter {
  implicit object DoubleConfigConverter extends ConfigConverter[Double] {
    def convertToConfig(a: Double) = ConfigDouble(a)
  }

  implicit object LongConfigConverter extends ConfigConverter[Long] {
    def convertToConfig(a: Long) = ConfigLong(a)
  }

  implicit object IntConfigConverter extends ConfigConverter[Int] {
    def convertToConfig(a: Int) = ConfigInt(a)
  }
  implicit object StringConfigConverter extends ConfigConverter[String] {
    def convertToConfig(a: String) = ConfigString(a)
  }

  implicit object BooleanConfigConverter extends ConfigConverter[Boolean] {
    def convertToConfig(a: Boolean) = ConfigBoolean(a)
  }

  implicit object ListConfigConverter extends ConfigConverter[List[ConfigValue]] {
    def convertToConfig(a: List[ConfigValue]) = ConfigList(a)
  }

  implicit object StringListConfigConverter extends ConfigConverter[List[String]] {
    def convertToConfig(a: List[String]) = ConfigList(a.map(ConfigString(_)))
  }

  implicit object StringCellConfigConverter extends ConfigConverter[StringCell] {
    def convertToConfig(a: StringCell) = a match {
      case StringCell(x) if x == ConfigMap.TRUE => true.toConfig
      case StringCell(x) if x == ConfigMap.FALSE => false.toConfig
      case StringCell(IntExtractor(x)) => x.toConfig
      case StringCell(LongExtractor(x)) => x.toConfig
      case StringCell(x) => x.toConfig
    }
  }

  def convert[A](x: A)(implicit ev: ConfigConverter[A]) = ev.convertToConfig(x)
}

