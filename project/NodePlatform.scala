import org.scalajs.sbtplugin.ScalaJSPlugin
import sbt.{Def, Project}
import sbtcrossproject._

object NodePlatform extends Platform {
  override def identifier: String = "node"

  override def sbtSuffix: String = "Node"

  override def enable(project: Project): Project = project.enablePlugins(ScalaJSPlugin)
}

object NodePlatformImplicits {
  implicit class NodeCrossProjectOps(project: CrossProject) {
    def node: Project = project.projects(NodePlatform)

    def nodeSettings(ss: Def.SettingsDefinition*): CrossProject =
      nodeConfigure(_.settings(ss: _*))

    def nodeConfigure(transformer: Project => Project): CrossProject =
      project.configurePlatform(NodePlatform)(transformer)
  }
}

