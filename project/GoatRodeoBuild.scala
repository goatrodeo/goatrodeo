import sbt._
import Keys._

object GoatRodeoBuild extends Build {

  lazy val grSettings = Project.defaultSettings ++ Seq(
    scalaVersion := "2.8.0",
    version := "0.1",
    organization := "org.stambecco",
    libraryDependencies ++= Seq(
      "net.liftweb" %% "lift-mapper" % "2.1",
      "com.rabbitmq" % "amqp-client" % "1.7.2",
      "org.scala-lang" % "scala-compiler" % "2.8.0",
      "junit" % "junit" % "4.7" % "test",
      "org.scala-tools.testing" %% "specs" % "1.6.5" % "test",
      "org.eclipse.jetty" % "jetty-webapp" % "7.0.2.v20100331" % "test"))

  lazy val foo = Project(
    id = "root",
    base = file("."),
    settings = grSettings) aggregate(coreProject, pluginProject, exampleSkittrProject, exampleTimeProject)

  lazy val coreProject = Project(
    base = file("core"),
    id = "stambecco-core",
    settings = grSettings)

  lazy val pluginProject = Project(
    base = file("plugin"),
    id = "stambecco-plugin",
    settings = grSettings) dependsOn (coreProject)

  lazy val exampleSkittrProject = Project(
    base = file("example-skittr"),
    id = "stambecco-example-skittr",
    settings = grSettings) dependsOn (coreProject)

  lazy val exampleTimeProject = Project(
    base = file("example-time"),
    id = "stambecco-example-time",
    settings = grSettings) dependsOn (coreProject)
}