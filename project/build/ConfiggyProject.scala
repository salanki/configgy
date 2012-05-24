import sbt._
import com.twitter.sbt._

class ConfiggyProject(info: ProjectInfo) extends StandardProject(info) with SubversionPublisher {
  override def disableCrossPaths = false

  val specs = buildScalaVersion match {
    case "2.8.1" => "org.scala-tools.testing" % "specs_2.8.1" % "1.6.6" % "test"
    case "2.9.2" => "org.scala-tools.testing" % "specs_2.9.1" % "1.6.9" % "test"
  }
  val json = buildScalaVersion match {
    case "2.7.7" => "com.twitter" % "json" % "1.1.7"
    case "2.8.1" => "com.twitter" % "json_2.8.1" % "2.1.6"
    case _ => "com.twitter" % "json_2.8.1" % "2.1.6"
  }

  override def pomExtra =
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>

  override def subversionRepository = Some("https://svn.twitter.biz/maven-public")
}
