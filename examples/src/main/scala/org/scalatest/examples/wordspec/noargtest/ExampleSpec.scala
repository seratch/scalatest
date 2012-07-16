package org.scalatest.examples.wordspec.noargtest

import java.io.File
import org.scalatest.WordSpec

class ExampleSpec extends WordSpec {

  final val tmpDir = "tmpDir"

  override def withFixture(test: NoArgTest) {

    try {
      super.withFixture(test)
    }
    catch {
      case e: Exception =>
        val currDir = new File(".")
        val fileNames = currDir.list()
        info("Dir snapshot: " + fileNames.mkString(", "))
        throw e
    }
  }

  "this test" should {
    "succeed" in {
      assert(1 + 1 === 2)
    }

    "fail" in {
      assert(1 + 1 === 3)
    }
  }
}