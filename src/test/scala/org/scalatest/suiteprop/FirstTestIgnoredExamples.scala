package org.scalatest.suiteprop

import org.scalatest._

class FirstTestIgnoredExamples extends SuiteExamples {

  trait Services {
    val theTestNames = Vector("first test", "second test")
  }

  type FixtureServices = Services

  class SuiteExample extends Suite with Services {
    @Ignore def testFirst {}
    def testSecond {}
    override val theTestNames = Vector("testFirst", "testSecond")
  }

  class FunSuiteExample extends FunSuite with Services {
    ignore("first test") {}
    test("second test") {}
  }

  class FixtureFunSuiteExample extends StringFixtureFunSuite with Services {
    ignore("first test") { s => }
    test("second test") { s => }
  }

  class FunSpecExample extends FunSpec with Services {
    ignore("first test") {}
    it("second test") {}
  }

  class FixtureSpecExample extends StringFixtureSpec with Services {
      ignore("first test") { s => }
      it("second test") { s => }
  }
  
  class PathFunSpecExample extends path.FunSpec with Services {
    ignore("first test") {}
    it("second test") {}
  }

  def suite = new SuiteExample
  def funSuite = new FunSuiteExample
  def fixtureFunSuite = new FixtureFunSuiteExample
  def funSpec = new FunSpecExample
  def fixtureFunSpec = new FixtureSpecExample
  def pathFunSpec = new PathFunSpecExample
}