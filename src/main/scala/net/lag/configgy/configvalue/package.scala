package net.lag.configgy

package object configvalue {
  implicit class ToConfig[A](x: A)(implicit ev: ConfigConverter[A]) {
    def toConfig: ConfigValue = ev.convertToConfig(x)
  }

  implicit class ToStringList[A: StringListConverter](x: A) {
    def toStringList = StringListConverter.convert(x)
  }
    
  implicit def configInt2Long(x: ConfigInt) = x.value
  implicit def configBool2Bool(x: ConfigBoolean) = x.value
  implicit def configString2String(x: ConfigString) = x.value
  implicit def configDouble2Double(x: ConfigDouble) = x.value
  implicit def configList2List[A <: ConfigValue](x: ConfigList[A]) = x.entries
  implicit def configObject2Map[A <: ConfigValue](x: ConfigObject[A]) = x.entries
}