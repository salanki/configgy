package net.lag.configgy
package configvalue

import scala.collection.immutable.TreeMap
import net.lag.extensions._


/**
 * Convert from standard types to ConfigValue wrapped ones
 */
trait ConfigConverter[A] {
  type Converted <: ConfigValue // Shouldn't be needed I think
  def convertToConfig(value: A): Converted
}

object ConfigConverter {
  implicit object DoubleConfigConverter extends ConfigConverter[Double] {
    type Converted = ConfigDouble
    def convertToConfig(a: Double): Converted = ConfigDouble(a)
  }

  implicit object LongConfigConverter extends ConfigConverter[Long] {
    type Converted = ConfigInt
    def convertToConfig(a: Long): Converted = ConfigInt(a)
  }

  implicit object IntConfigConverter extends ConfigConverter[Int] {
    type Converted = ConfigInt
    def convertToConfig(a: Int) = ConfigInt(a)
  }
  implicit object StringConfigConverter extends ConfigConverter[String] {
    type Converted = ConfigString
    def convertToConfig(a: String) = ConfigString(a)
  }

  implicit object BooleanConfigConverter extends ConfigConverter[Boolean] {
    type Converted = ConfigBoolean
    def convertToConfig(a: Boolean) = ConfigBoolean(a)
  }

  implicit object ListConfigConverter extends ConfigConverter[List[ConfigValue]] {
    type Converted = ConfigList[ConfigValue]
    def convertToConfig(a: List[ConfigValue]) = ConfigList(a)
  }

  implicit object StringListConfigConverter extends ConfigConverter[List[String]] {
    type Converted = ConfigList[ConfigString]
    def convertToConfig(a: List[String]) = ConfigList(a.map(ConfigString(_)))
  }

  implicit object StringCellConfigConverter extends ConfigConverter[StringCell] {
    type Converted = ConfigValue
    
    def convertToConfig(a: StringCell): ConfigValue = a match {
      case StringCell(x) if x == ConfigMap.TRUE => convert(true)
      case StringCell(x) if x == ConfigMap.FALSE => convert(false)
      case StringCell(IntExtractor(x)) => convert(x)
      case StringCell(LongExtractor(x)) => convert(x)
      case StringCell(x) => convert(x)
    }
  }

  def convert[A](x: A)(implicit ev: ConfigConverter[A]): ev.Converted = ev.convertToConfig(x)
}

