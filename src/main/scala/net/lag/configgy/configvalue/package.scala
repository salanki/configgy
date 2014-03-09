package net.lag.configgy

package object configvalue {
  implicit class ToConfig[A: ConfigConverter](x: A) {
    def toConfig = ConfigConverter.convert(x)
  }

  implicit class ToStringList[A: StringListConverter](x: A) {
    def toStringList = StringListConverter.convert(x)
  }
  
  implicit def ConfigInt2ConfigLong(x: ConfigInt): ConfigLong = x.toLong
}