import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
<<<<<<< HEAD
  val scalaTools = "scala-tools.org" at "http://scala-tools.org/repo-releases/"
  val lagNet = "twitter.com" at "http://www.lag.net/repo/"
  val defaultProject = "com.twitter" % "standard-project" % "0.7.16"
=======
  val twitter = "twitter.com" at "http://maven.twttr.com"
  val defaultProject = "com.twitter" % "standard-project" % "0.7.10"
>>>>>>> f9c1639208e51f0b9a0db65f396ad37310292aae
}
