name := "scalatest-cucumber-runner"
 
version := "0.2.0"
 
scalaVersion := "2.9.0"

parallelExecution in Test := false

libraryDependencies ++= Seq(
	"org.scalatest" % "scalatest_2.9.0" % "2.0.M2", 
	"info.cukes" % "cucumber-java" % "1.0.0.RC24", 
	"info.cukes" % "cucumber-scala" % "1.0.0.RC24"
)