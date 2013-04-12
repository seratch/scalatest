name := "scalatest-finders"
 
organization := "org.scalatest"

version := "0.9.5"

scalaVersion := "2.9.0"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "2.0.M6-SNAP4" % "test"

resolvers ++= Seq("releases" at "http://oss.sonatype.org/content/repositories/releases",
                  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots", 
                  "localmaven" at "file://"+Path.userHome+"/.m2/repository")

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) Some("publish-snapshots" at nexus + "content/repositories/snapshots")
  else                             Some("publish-releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://www.scalatest.org</url>
  <licenses>
    <license>
      <name>the Apache License, ASL Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>http://code.google.com/p/scalatest/source/browse/</url>
    <connection>scm:svn:http://scalatest.googlecode.com/svn/trunk/</connection>
    <developerConnection>
      scm:svn:http://scalatest.googlecode.com/svn/trunk/
    </developerConnection>
  </scm>
  <developers>
    <developer>
      <id>bill</id>
      <name>Bill Venners</name>
      <email>bill@artima.com</email>
    </developer>
  </developers>
)

