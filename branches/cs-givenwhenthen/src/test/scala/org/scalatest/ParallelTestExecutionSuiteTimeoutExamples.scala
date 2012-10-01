package org.scalatest

import org.scalatest.events.Event
import org.scalatest.prop.Tables

trait SuiteTimeoutSuites extends EventHelpers {
  def suite1: Suite
  def suite2: Suite
  def assertSuiteTimeoutTest(events: List[Event])
}

trait ParallelTestExecutionSuiteTimeoutExamples extends Tables {
  
  def suiteTimeoutExamples = 
    Table(
      "pair", 
      new ExampleParallelTestExecutionSuiteTimeoutSuitePair, 
      new ExampleParallelTestExecutionSuiteTimeoutSpecPair, 
      new ExampleParallelTestExecutionSuiteTimeoutFunSuitePair, 
      new ExampleParallelTestExecutionSuiteTimeoutFunSpecPair, 
      new ExampleParallelTestExecutionSuiteTimeoutFeatureSpecPair,
      new ExampleParallelTestExecutionSuiteTimeoutFlatSpecPair,
      new ExampleParallelTestExecutionSuiteTimeoutFreeSpecPair,
      new ExampleParallelTestExecutionSuiteTimeoutPropSpecPair,
      new ExampleParallelTestExecutionSuiteTimeoutWordSpecPair
    )
}

class ExampleParallelTestExecutionSuiteTimeoutSuitePair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutSuite
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureSuite
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "testMethod1")
    checkTestSucceeded(events(2), "testMethod1")
    checkTestStarting(events(3), "testMethod2")
    checkTestSucceeded(events(4), "testMethod2")
    checkSuiteCompleted(events(5), suite1.suiteId)
    
    checkSuiteStarting(events(6), suite2.suiteId)
    checkTestStarting(events(7), "testFixtureMethod1")
    checkTestSucceeded(events(8), "testFixtureMethod1")
    checkTestStarting(events(9), "testFixtureMethod2")
    checkTestSucceeded(events(10), "testFixtureMethod2")
    checkTestStarting(events(11), "testFixtureMethod3")
    checkTestSucceeded(events(12), "testFixtureMethod3")
    checkSuiteCompleted(events(13), suite2.suiteId)
    
    checkTestStarting(events(14), "testMethod3")
    checkTestSucceeded(events(15), "testMethod3")
  }
}

class ExampleParallelTestExecutionSuiteTimeoutSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureSpec
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "test 1")
    checkTestSucceeded(events(2), "test 1")
    checkTestStarting(events(3), "test 2")
    checkTestSucceeded(events(4), "test 2")
    checkSuiteCompleted(events(5), suite1.suiteId)
    
    checkSuiteStarting(events(6), suite2.suiteId)
    checkTestStarting(events(7), "test 1")
    checkTestSucceeded(events(8), "test 1")
    checkTestStarting(events(9), "test 2")
    checkTestSucceeded(events(10), "test 2")
    checkTestStarting(events(11), "test 3")
    checkTestSucceeded(events(12), "test 3")
    checkSuiteCompleted(events(13), suite2.suiteId)
    
    checkTestStarting(events(14), "test 3")
    checkTestSucceeded(events(15), "test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutSuite extends Suite with ParallelTestExecution {
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() { Thread.sleep(300) }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureSuite extends fixture.Suite with ParallelTestExecution with StringFixture {
  def testFixtureMethod1() {}
  def testFixtureMethod2() {}
  def testFixtureMethod3() {}
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutSpec extends Spec with ParallelTestExecution {
  def `test 1` {}
  def `test 2` {}
  def `test 3` { Thread.sleep(300) }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureSpec extends fixture.Spec with ParallelTestExecution with StringFixture {
  def `test 1`(fixture: String) {}
  def `test 2`(fixture: String) {}
  def `test 3`(fixture: String) {}
}

class ExampleParallelTestExecutionSuiteTimeoutFunSuitePair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFunSuite
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFunSuite
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkTestStarting(events(3), "Test 2")
    checkTestSucceeded(events(4), "Test 2")
    checkSuiteCompleted(events(5), suite1.suiteId)
    
    checkSuiteStarting(events(6), suite2.suiteId)
    checkTestStarting(events(7), "Fixture Test 1")
    checkTestSucceeded(events(8), "Fixture Test 1")
    checkTestStarting(events(9), "Fixture Test 2")
    checkTestSucceeded(events(10), "Fixture Test 2")
    checkTestStarting(events(11), "Fixture Test 3")
    checkTestSucceeded(events(12), "Fixture Test 3")
    checkSuiteCompleted(events(13), suite2.suiteId)
    
    checkTestStarting(events(14), "Test 3")
    checkTestSucceeded(events(15), "Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFunSuite extends FunSuite with ParallelTestExecution {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") { Thread.sleep(300) }
  
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFunSuite extends fixture.FunSuite with ParallelTestExecution with StringFixture {
  test("Fixture Test 1") { fixture => }
  test("Fixture Test 2") { fixture => }
  test("Fixture Test 3") { fixture => }
}

class ExampleParallelTestExecutionSuiteTimeoutFunSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFunSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFunSpec
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Scope 1")
    checkTestStarting(events(2), "Scope 1 Test 1")
    checkTestSucceeded(events(3), "Scope 1 Test 1")
    checkTestStarting(events(4), "Scope 1 Test 2")
    checkTestSucceeded(events(5), "Scope 1 Test 2")
    checkScopeClosed(events(6), "Scope 1")
    checkScopeOpened(events(7), "Scope 2")
    checkTestStarting(events(8), "Scope 2 Test 3")
    checkTestSucceeded(events(9), "Scope 2 Test 3")
    checkSuiteCompleted(events(10), suite1.suiteId)
    
    checkSuiteStarting(events(11), suite2.suiteId)
    checkScopeOpened(events(12), "Fixture Scope 1")
    checkTestStarting(events(13), "Fixture Scope 1 Fixture Test 1")
    checkTestSucceeded(events(14), "Fixture Scope 1 Fixture Test 1")
    checkTestStarting(events(15), "Fixture Scope 1 Fixture Test 2")
    checkTestSucceeded(events(16), "Fixture Scope 1 Fixture Test 2")
    checkScopeClosed(events(17), "Fixture Scope 1")
    checkScopeOpened(events(18), "Fixture Scope 2")
    checkTestStarting(events(19), "Fixture Scope 2 Fixture Test 3")
    checkTestSucceeded(events(20), "Fixture Scope 2 Fixture Test 3")
    checkTestStarting(events(21), "Fixture Scope 2 Fixture Test 4")
    checkTestSucceeded(events(22), "Fixture Scope 2 Fixture Test 4")
    checkScopeClosed(events(23), "Fixture Scope 2")
    checkSuiteCompleted(events(24), suite2.suiteId)
    
    checkTestStarting(events(25), "Scope 2 Test 4")
    checkTestSucceeded(events(26), "Scope 2 Test 4")
    checkScopeClosed(events(27), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFunSpec extends FunSpec with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
  }
  describe("Scope 2") {
    it("Test 3") {}
    it("Test 4") { Thread.sleep(300) }
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFunSpec extends fixture.FunSpec with ParallelTestExecution with StringFixture {
  describe("Fixture Scope 1") {
    it("Fixture Test 1") { fixture => }
    it("Fixture Test 2") { fixture => }
  }
  describe("Fixture Scope 2") {
    it("Fixture Test 3") { fixture => }
    it("Fixture Test 4") { fixture => }
  }
}

class ExampleParallelTestExecutionSuiteTimeoutFeatureSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFeatureSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFeatureSpec
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Feature: Scope 1")
    checkTestStarting(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(3), "Feature: Scope 1 Scenario: Test 1")
    checkTestStarting(events(4), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(5), "Feature: Scope 1 Scenario: Test 2")
    checkScopeClosed(events(6), "Feature: Scope 1")
    checkScopeOpened(events(7), "Feature: Scope 2")
    checkTestStarting(events(8), "Feature: Scope 2 Scenario: Test 3")
    checkTestSucceeded(events(9), "Feature: Scope 2 Scenario: Test 3")
    checkSuiteCompleted(events(10), suite1.suiteId)
    
    checkSuiteStarting(events(11), suite2.suiteId)
    checkScopeOpened(events(12), "Feature: Fixture Scope 1")
    checkTestStarting(events(13), "Feature: Fixture Scope 1 Scenario: Fixture Test 1")
    checkTestSucceeded(events(14), "Feature: Fixture Scope 1 Scenario: Fixture Test 1")
    checkTestStarting(events(15), "Feature: Fixture Scope 1 Scenario: Fixture Test 2")
    checkTestSucceeded(events(16), "Feature: Fixture Scope 1 Scenario: Fixture Test 2")
    checkScopeClosed(events(17), "Feature: Fixture Scope 1")
    checkScopeOpened(events(18), "Feature: Fixture Scope 2")
    checkTestStarting(events(19), "Feature: Fixture Scope 2 Scenario: Fixture Test 3")
    checkTestSucceeded(events(20), "Feature: Fixture Scope 2 Scenario: Fixture Test 3")
    checkTestStarting(events(21), "Feature: Fixture Scope 2 Scenario: Fixture Test 4")
    checkTestSucceeded(events(22), "Feature: Fixture Scope 2 Scenario: Fixture Test 4")
    checkScopeClosed(events(23), "Feature: Fixture Scope 2")
    checkSuiteCompleted(events(24), suite2.suiteId)
    
    checkTestStarting(events(25), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(26), "Feature: Scope 2 Scenario: Test 4")
    checkScopeClosed(events(27), "Feature: Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFeatureSpec extends FeatureSpec with ParallelTestExecution {
  Feature("Scope 1") {
    Scenario("Test 1") {}
    Scenario("Test 2") {}
  }
  Feature("Scope 2") {
    Scenario("Test 3") {}
    Scenario("Test 4") { Thread.sleep(300) }
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFeatureSpec extends fixture.FeatureSpec with ParallelTestExecution with StringFixture {
  Feature("Fixture Scope 1") {
    Scenario("Fixture Test 1") { fixture => }
    Scenario("Fixture Test 2") { fixture =>}
  }
  Feature("Fixture Scope 2") {
    Scenario("Fixture Test 3") { fixture => }
    Scenario("Fixture Test 4") { fixture => }
  }
}

class ExampleParallelTestExecutionSuiteTimeoutFlatSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFlatSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFlatSpec
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Scope 1")
    checkTestStarting(events(2), "Scope 1 should Test 1")
    checkTestSucceeded(events(3), "Scope 1 should Test 1")
    checkTestStarting(events(4), "Scope 1 should Test 2")
    checkTestSucceeded(events(5), "Scope 1 should Test 2")
    checkScopeClosed(events(6), "Scope 1")
    checkScopeOpened(events(7), "Scope 2")
    checkTestStarting(events(8), "Scope 2 should Test 3")
    checkTestSucceeded(events(9), "Scope 2 should Test 3")    
    checkSuiteCompleted(events(10), suite1.suiteId)
    
    checkSuiteStarting(events(11), suite2.suiteId)
    checkScopeOpened(events(12), "Fixture Scope 1")
    checkTestStarting(events(13), "Fixture Scope 1 should Fixture Test 1")
    checkTestSucceeded(events(14), "Fixture Scope 1 should Fixture Test 1")
    checkTestStarting(events(15), "Fixture Scope 1 should Fixture Test 2")
    checkTestSucceeded(events(16), "Fixture Scope 1 should Fixture Test 2")
    checkScopeClosed(events(17), "Fixture Scope 1")
    checkScopeOpened(events(18), "Fixture Scope 2")
    checkTestStarting(events(19), "Fixture Scope 2 should Fixture Test 3")
    checkTestSucceeded(events(20), "Fixture Scope 2 should Fixture Test 3")
    checkTestStarting(events(21), "Fixture Scope 2 should Fixture Test 4")
    checkTestSucceeded(events(22), "Fixture Scope 2 should Fixture Test 4")
    checkScopeClosed(events(23), "Fixture Scope 2")
    checkSuiteCompleted(events(24), suite2.suiteId)
    
    checkTestStarting(events(25), "Scope 2 should Test 4")
    checkTestSucceeded(events(26), "Scope 2 should Test 4")
    checkScopeClosed(events(27), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFlatSpec extends FlatSpec with ParallelTestExecution {
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in {}
  it should "Test 4" in { Thread.sleep(300) }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFlatSpec extends fixture.FlatSpec with ParallelTestExecution with StringFixture {
  behavior of "Fixture Scope 1"
  it should "Fixture Test 1" in { fixture => }
  it should "Fixture Test 2" in { fixture => }
  
  behavior of "Fixture Scope 2"
  it should "Fixture Test 3" in { fixture => }
  it should "Fixture Test 4" in { fixture => }
}

class ExampleParallelTestExecutionSuiteTimeoutFreeSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutFreeSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureFreeSpec
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Scope 1")
    checkTestStarting(events(2), "Scope 1 Test 1")
    checkTestSucceeded(events(3), "Scope 1 Test 1")
    checkTestStarting(events(4), "Scope 1 Test 2")
    checkTestSucceeded(events(5), "Scope 1 Test 2")
    checkScopeClosed(events(6), "Scope 1")
    checkScopeOpened(events(7), "Scope 2")
    checkTestStarting(events(8), "Scope 2 Test 3")
    checkTestSucceeded(events(9), "Scope 2 Test 3")
    checkSuiteCompleted(events(10), suite1.suiteId)
    
    checkSuiteStarting(events(11), suite2.suiteId)
    checkScopeOpened(events(12), "Fixture Scope 1")
    checkTestStarting(events(13), "Fixture Scope 1 Fixture Test 1")
    checkTestSucceeded(events(14), "Fixture Scope 1 Fixture Test 1")
    checkTestStarting(events(15), "Fixture Scope 1 Fixture Test 2")
    checkTestSucceeded(events(16), "Fixture Scope 1 Fixture Test 2")
    checkScopeClosed(events(17), "Fixture Scope 1")
    checkScopeOpened(events(18), "Fixture Scope 2")
    checkTestStarting(events(19), "Fixture Scope 2 Fixture Test 3")
    checkTestSucceeded(events(20), "Fixture Scope 2 Fixture Test 3")
    checkTestStarting(events(21), "Fixture Scope 2 Fixture Test 4")
    checkTestSucceeded(events(22), "Fixture Scope 2 Fixture Test 4")
    checkScopeClosed(events(23), "Fixture Scope 2")
    checkSuiteCompleted(events(24), suite2.suiteId)
    
    checkTestStarting(events(25), "Scope 2 Test 4")
    checkTestSucceeded(events(26), "Scope 2 Test 4")
    checkScopeClosed(events(27), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFreeSpec extends FreeSpec with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" - {
    "Test 3" in {}
    "Test 4" in { Thread.sleep(300) }
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureFreeSpec extends fixture.FreeSpec with ParallelTestExecution with StringFixture {
  "Fixture Scope 1" - {
    "Fixture Test 1" in { fixture => }
    "Fixture Test 2" in { fixture => }
  }
  
  "Fixture Scope 2" - {
    "Fixture Test 3" in { fixture => }
    "Fixture Test 4" in { fixture => }
  }
}

class ExampleParallelTestExecutionSuiteTimeoutPropSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutPropSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixturePropSpec
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 16)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkTestStarting(events(1), "Test 1")
    checkTestSucceeded(events(2), "Test 1")
    checkTestStarting(events(3), "Test 2")
    checkTestSucceeded(events(4), "Test 2")
    checkSuiteCompleted(events(5), suite1.suiteId)
    
    checkSuiteStarting(events(6), suite2.suiteId)
    checkTestStarting(events(7), "Fixture Test 1")
    checkTestSucceeded(events(8), "Fixture Test 1")
    checkTestStarting(events(9), "Fixture Test 2")
    checkTestSucceeded(events(10), "Fixture Test 2")
    checkTestStarting(events(11), "Fixture Test 3")
    checkTestSucceeded(events(12), "Fixture Test 3")
    checkSuiteCompleted(events(13), suite2.suiteId)
    
    checkTestStarting(events(14), "Test 3")
    checkTestSucceeded(events(15), "Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutPropSpec extends PropSpec with ParallelTestExecution {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") { Thread.sleep(300) }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixturePropSpec extends fixture.PropSpec with ParallelTestExecution with StringFixture {
  property("Fixture Test 1") { fixture => }
  property("Fixture Test 2") { fixture => }
  property("Fixture Test 3") { fixture => }
}

class ExampleParallelTestExecutionSuiteTimeoutWordSpecPair extends SuiteTimeoutSuites {
  def suite1 = new ExampleParallelTestExecutionSuiteTimeoutWordSpec
  def suite2 = new ExampleParallelTestExecutionSuiteTimeoutFixtureWordSpec
  
  def assertSuiteTimeoutTest(events: List[Event]) {
    assert(events.size === 28)
    
    checkSuiteStarting(events(0), suite1.suiteId)
    checkScopeOpened(events(1), "Scope 1")
    checkTestStarting(events(2), "Scope 1 should Test 1")
    checkTestSucceeded(events(3), "Scope 1 should Test 1")
    checkTestStarting(events(4), "Scope 1 should Test 2")
    checkTestSucceeded(events(5), "Scope 1 should Test 2")
    checkScopeClosed(events(6), "Scope 1")
    checkScopeOpened(events(7), "Scope 2")
    checkTestStarting(events(8), "Scope 2 should Test 3")
    checkTestSucceeded(events(9), "Scope 2 should Test 3")
    checkSuiteCompleted(events(10), suite1.suiteId)
    
    checkSuiteStarting(events(11), suite2.suiteId)
    checkScopeOpened(events(12), "Fixture Scope 1")
    checkTestStarting(events(13), "Fixture Scope 1 should Fixture Test 1")
    checkTestSucceeded(events(14), "Fixture Scope 1 should Fixture Test 1")
    checkTestStarting(events(15), "Fixture Scope 1 should Fixture Test 2")
    checkTestSucceeded(events(16), "Fixture Scope 1 should Fixture Test 2")
    checkScopeClosed(events(17), "Fixture Scope 1")
    checkScopeOpened(events(18), "Fixture Scope 2")
    checkTestStarting(events(19), "Fixture Scope 2 should Fixture Test 3")
    checkTestSucceeded(events(20), "Fixture Scope 2 should Fixture Test 3")
    checkTestStarting(events(21), "Fixture Scope 2 should Fixture Test 4")
    checkTestSucceeded(events(22), "Fixture Scope 2 should Fixture Test 4")
    checkScopeClosed(events(23), "Fixture Scope 2")
    checkSuiteCompleted(events(24), suite2.suiteId)
    
    checkTestStarting(events(25), "Scope 2 should Test 4")
    checkTestSucceeded(events(26), "Scope 2 should Test 4")
    checkScopeClosed(events(27), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutWordSpec extends WordSpec with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" should {
    "Test 3" in {}
    "Test 4" in { Thread.sleep(300) }
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionSuiteTimeoutFixtureWordSpec extends fixture.WordSpec with ParallelTestExecution with StringFixture {
  "Fixture Scope 1" should {
    "Fixture Test 1" in { fixture => }
    "Fixture Test 2" in { fixture => }
  }
  
  "Fixture Scope 2" should {
    "Fixture Test 3" in { fixture => }
    "Fixture Test 4" in { fixture => }
  }
}
