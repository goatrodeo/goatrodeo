import sbt._
import Process._

class Project(info: ProjectInfo) extends DefaultProject(info) {

  // Some properties
  lazy val LiftVersion = "2.0-scala280-SNAPSHOT"

  // Snapshot Repositories => To be removed somewhen!!
  val liftModuleConfig = ModuleConfiguration("net.liftweb", ScalaToolsSnapshots)

  // Dependencies as defs(!!) here
  val liftJson =    "net.liftweb" % "lift-json"    % LiftVersion
  val liftUtils =   "net.liftweb" % "lift-util"    % LiftVersion
  val liftTestkit = "net.liftweb" % "lift-testkit" % LiftVersion

  // Other stuff
  override val mainClass = Some("org.stambecco.harness.Run")
}
