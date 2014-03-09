package net.lag.configgy

object IntExtractor {
  def unapply(x: String): Option[Int] =
    try {
      Some(x.toInt)
    } catch {
      case _: NumberFormatException => None
    }
}

object LongExtractor {
  def unapply(x: String): Option[Long] =
    try {
      Some(x.toLong)
    } catch {
      case _: NumberFormatException => None
    }
}

object BooleanExtractor {
  def unapply(x: String): Option[Boolean] = x match {
    case ConfigMap.TRUE => Some(true)
    case ConfigMap.FALSE => Some(false)
    case _ => None
  }
}