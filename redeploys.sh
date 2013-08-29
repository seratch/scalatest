set -x
ant redeploy -Dold.scala.version='2.9.0' -Dnew.scala.version='2.9.3' -Dscalatest.version=1.9.2-RC1
ant redeploy -Dold.scala.version='2.9.0' -Dnew.scala.version='2.9.2' -Dscalatest.version=1.9.2-RC1
ant redeploy -Dold.scala.version='2.9.0' -Dnew.scala.version='2.9.1' -Dscalatest.version=1.9.2-RC1
ant redeploy -Dold.scala.version='2.9.0' -Dnew.scala.version='2.9.1-1' -Dscalatest.version=1.9.2-RC1
ant redeploy -Dold.scala.version='2.9.0' -Dnew.scala.version='2.9.0-1' -Dscalatest.version=1.9.2-RC1

