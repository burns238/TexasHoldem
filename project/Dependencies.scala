import sbt._

object Dependencies extends NonProductionDependencies

trait NonProductionDependencies  {

  val scalatestDependency = "org.scalatest" %% "scalatest" % "3.0.5"
  
}
