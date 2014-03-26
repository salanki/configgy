//import CoverallsPlugin.CoverallsKeys._

ScoverageSbtPlugin.instrumentSettings

CoverallsPlugin.coverallsSettings

//coverallsTokenFile := "token.txt"

name := "Configgy"

version := "2.1.0-SALANKI"

organization := "net.lag"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
 "com.twitter" % "json_2.8.1" % "2.1.6",
 "de.congrace" % "exp4j" % "0.3.8"
)

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0" % "test"

initialCommands := """
  import System.{currentTimeMillis => now}
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
  }
"""
