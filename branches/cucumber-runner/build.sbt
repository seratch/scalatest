name := "scalatest-cucumber-runner"
 
version := "0.1.0"
 
scalaVersion := "2.9.0"

parallelExecution in Test := false

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "1.7.1", 
	"info.cukes" % "cucumber-java" % "1.0.0.RC24"
)