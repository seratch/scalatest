name := "scalatest-spec-runner"
 
version := "0.4.0"
 
scalaVersion := "2.9.0"

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scala-tools.testing" % "specs_2.9.0" % "1.6.8"
)

resolvers += "Sonatype Maven Repository" at "http://oss.sonatype.org/content/repositories/snapshots"