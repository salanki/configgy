package net.lag.configgy
package configvalue

import scala.collection.immutable.TreeMap

// TODO: PROBABLY NOT NEEDED, REMOVE

/**
 * Convert from a ConfigValue type to a List[String]
 */
trait StringListConverter[A] {
  def convertToStringList(value: A): List[String]
}

object StringListConverter {
  implicit object StringConfigConverter extends StringListConverter[ConfigString] {
    def convertToStringList(a: ConfigString) = a.value :: Nil
  }

  implicit object BooleanConfigConverter extends StringListConverter[ConfigBoolean] {
    def convertToStringList(a: ConfigBoolean) = a.value.toString :: Nil
  }

  implicit object ListConfigConverter extends StringListConverter[ConfigList[_]] {
    def convertToStringList(a: ConfigList[_]) = a.entries match {
      case ls: List[ConfigString] => ls.map(_.value)
    }
  } 

  def convert[A](x: A)(implicit ev: StringListConverter[A]) = ev.convertToStringList(x)
}

