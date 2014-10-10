scoverage.ScoverageSbtPlugin.instrumentSettings

org.scoverage.coveralls.CoverallsPlugin.coverallsSettings

//coverallsTokenFile := "token.txt"

name := "Configgy"

version := "2.1.0-SALANKI"

organization := "net.lag"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
 "de.congrace" % "exp4j" % "0.3.8",
 "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

initialCommands := """
  import System.{currentTimeMillis => now}
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
  }
"""
