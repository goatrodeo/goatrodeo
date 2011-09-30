import sbt._

/** The very very parent project of Stambecco. */
class StambeccoParentProject(info: ProjectInfo) extends ParentProject(info) {

  // Some properties
  lazy val LiftVersion = "2.0-scala280-SNAPSHOT"

  // Snapshot Repositories => To be removed somewhen!!
  val liftModuleConfig = ModuleConfiguration("net.liftweb",             ScalaToolsSnapshots)
  val specsConfig      = ModuleConfiguration("org.scala-tools.testing", ScalaToolsSnapshots)

  // Dependencies as defs(!!) here
  def liftMapper   = "net.liftweb"             % "lift-mapper"     % LiftVersion
  def rabbitMqAmqp = "com.rabbitmq"            % "amqp-client"     % "1.7.2"
  def junit        = "junit"                   % "junit"           % "4.7"             % "test" // Attention: Use the right version for specs!
  def specs        = "org.scala-tools.testing" % "specs_2.8.0.RC3" % "1.6.5-SNAPSHOT"  % "test"
  def jettyWebapp  = "org.eclipse.jetty"       % "jetty-webapp"    % "7.0.2.v20100331" % "test"

  // Fix for issue 99 (is this necessary at all?)
  // override def deliverAction = super.publishAction dependsOn(publishLocal)

  // Subprojects
  lazy val coreProject          = project("core",           "stambecco-core",           new CoreProject(_))
  lazy val pluginProject        = project("plugin",         "stambecco-plugin",         new PluginProject(_))
  lazy val exampleSkittrProject = project("example-skittr", "stambecco-example-skittr", new ExampleSkittrProject(_), coreProject)
  lazy val exampleTimeProject   = project("example-time",   "stambecco-example-time",   new ExampleTimeProject(_),   coreProject)

  /** core subproject. */
  class CoreProject(info: ProjectInfo) extends DefaultProject(info) {
    override def libraryDependencies = Set(liftMapper, rabbitMqAmqp, junit, specs)
  }

  /** plugin subproject. */
  class PluginProject(info: ProjectInfo) extends DefaultProject(info) {
  }

  /** example-skittr subproject. */
  class ExampleSkittrProject(info: ProjectInfo) extends DefaultWebProject(info) {
    override def libraryDependencies = Set(jettyWebapp)
    override def compileAction = super.compileAction dependsOn pluginProject.`package`
    override def compileOptions = super.compileOptions ++ compileOptions("-Xplugin:" + pluginProject.jarPath.absolutePath)
  }

  /** example-time subproject. */
  class ExampleTimeProject(info: ProjectInfo) extends DefaultProject(info) {
    override def compileAction = super.compileAction dependsOn pluginProject.`package`
    override def compileOptions = super.compileOptions ++ compileOptions("-Xplugin:" + pluginProject.jarPath.absolutePath)
  }
}
