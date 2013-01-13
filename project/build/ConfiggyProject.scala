import sbt._
import com.twitter.sbt._

class ConfiggyProject(info: ProjectInfo) extends StandardProject(info) with SubversionPublisher {
  /******** Publishing *******/
  override def managedStyle = ManagedStyle.Maven
  val publishTo = Resolver.file("maven-local", Path.userHome / ".m2" / "repository" asFile)

  val specs = "org.scala-tools.testing" % "specs_2.9.0-1" % "1.6.8" % "test"
  val json = buildScalaVersion match {
    case "2.7.7" => "com.twitter" % "json" % "1.1.7"
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

  override def subversionRepository = Some("http://svn.local.twitter.com/maven-public")
}
