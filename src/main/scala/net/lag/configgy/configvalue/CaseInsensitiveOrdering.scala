package net.lag.configgy
package configvalue

object CaseInsensitiveOrdering extends Ordering[String] {
  def compare(a: String, b: String) = a.compareToIgnoreCase(b)
}