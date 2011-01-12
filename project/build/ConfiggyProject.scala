import sbt._
import com.twitter.sbt._

class ConfiggyProject(info: ProjectInfo) extends StandardProject(info) with SubversionPublisher with InlineDependencies {
  inline("com.twitter" % "json" % "1.1.7")
  val specs = "org.scala-tools.testing" % "specs" % "1.6.2.1" % "test"

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
