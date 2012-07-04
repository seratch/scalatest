package org.scalatest

import org.scalatest.prop.Tables
import org.scalatest.events.Event

trait OrderExpectedResults extends EventHelpers {
  def assertOrderTest(events: List[Event])
}

trait ParallelTestExecutionOrderExamples extends Tables {

  def orderSuite = new ExampleParallelTestExecutionOrderSuite
  def orderFixtureSuite = new ExampleParallelTestExecutionOrderFixtureSuite
  def orderFunSuite = new ExampleParallelTestExecutionOrderFunSuite
  def orderFixtureFunSuite = new ExampleParallelTestExecutionOrderFixtureFunSuite
  def orderFunSpec = new ExampleParallelTestExecutionOrderFunSpec
  def orderFixtureFunSpec = new ExampleParallelTestExecutionOrderFixtureFunSpec
  def orderFeatureSpec = new ExampleParallelTestExecutionOrderFeatureSpec
  def orderFixtureFeatureSpec = new ExampleParallelTestExecutionOrderFixtureFeatureSpec
  def orderFlatSpec = new ExampleParallelTestExecutionOrderFlatSpec
  def orderFixtureFlatSpec = new ExampleParallelTestExecutionOrderFixtureFlatSpec
  def orderFreeSpec = new ExampleParallelTestExecutionOrderFreeSpec
  def orderFixtureFreeSpec = new ExampleParallelTestExecutionOrderFixtureFreeSpec
  def orderPropSpec = new ExampleParallelTestExecutionOrderPropSpec
  def orderFixturePropSpec = new ExampleParallelTestExecutionOrderFixturePropSpec
  def orderWordSpec = new ExampleParallelTestExecutionOrderWordSpec
  def orderFixtureWordSpec = new ExampleParallelTestExecutionOrderFixtureWordSpec
  
  def orderExamples =
    Table(
      "suite1",
      orderSuite, 
      orderFixtureSuite, 
      orderFunSuite, 
      orderFixtureFunSuite, 
      orderFunSpec, 
      orderFixtureFunSpec, 
      orderFeatureSpec, 
      orderFixtureFeatureSpec, 
      orderFlatSpec, 
      orderFixtureFlatSpec, 
      orderFreeSpec, 
      orderFixtureFreeSpec,
      orderPropSpec, 
      orderFixturePropSpec, 
      orderWordSpec, 
      orderFixtureWordSpec
    )
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderSuite extends Suite with OrderExpectedResults with ParallelTestExecution {
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "testMethod1")
    checkTestSucceeded(events(1), "testMethod1")
    checkTestStarting(events(2), "testMethod2")
    checkTestSucceeded(events(3), "testMethod2")
    checkTestStarting(events(4), "testMethod3")
    checkTestSucceeded(events(5), "testMethod3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFixtureSuite extends fixture.Suite with OrderExpectedResults with ParallelTestExecution with StringFixture {
  def testMethod1() {}
  def testMethod2() {}
  def testMethod3() {}
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "testMethod1")
    checkTestSucceeded(events(1), "testMethod1")
    checkTestStarting(events(2), "testMethod2")
    checkTestSucceeded(events(3), "testMethod2")
    checkTestStarting(events(4), "testMethod3")
    checkTestSucceeded(events(5), "testMethod3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFunSuite extends FunSuite with OrderExpectedResults with ParallelTestExecution {
  test("Test 1") {}
  test("Test 2") {}
  test("Test 3") {}
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestSucceeded(events(3), "Test 2")
    checkTestStarting(events(4), "Test 3")
    checkTestSucceeded(events(5), "Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFixtureFunSuite extends fixture.FunSuite with OrderExpectedResults with ParallelTestExecution with StringFixture {
  test("Test 1") { fixture => }
  test("Test 2") { fixture => }
  test("Test 3") { fixture => }
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestSucceeded(events(3), "Test 2")
    checkTestStarting(events(4), "Test 3")
    checkTestSucceeded(events(5), "Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFunSpec extends FunSpec with OrderExpectedResults with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
  }
  describe("Scope 2") {
    it("Test 3") {}
    it("Test 4") {}
  }
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestSucceeded(events(8), "Scope 2 Test 3")
    checkTestStarting(events(9), "Scope 2 Test 4")
    checkTestSucceeded(events(10), "Scope 2 Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFixtureFunSpec extends fixture.FunSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  describe("Scope 1") {
    it("Test 1") { fixture => }
    it("Test 2") { fixture =>}
  }
  describe("Scope 2") {
    it("Test 3") { fixture => }
    it("Test 4") { fixture =>}
  }
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestSucceeded(events(8), "Scope 2 Test 3")
    checkTestStarting(events(9), "Scope 2 Test 4")
    checkTestSucceeded(events(10), "Scope 2 Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFeatureSpec extends FeatureSpec with OrderExpectedResults with ParallelTestExecution {
  feature("Scope 1") {
    scenario("Test 1") {}
    scenario("Test 2") {}
  }
  feature("Scope 2") {
    scenario("Test 3") {}
    scenario("Test 4") {}
  }
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(2), "Scope 1 Scenario: Test 1")
    checkTestStarting(events(3), "Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(4), "Scope 1 Scenario: Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Scenario: Test 3")
    checkTestSucceeded(events(8), "Scope 2 Scenario: Test 3")
    checkTestStarting(events(9), "Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(10), "Scope 2 Scenario: Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFixtureFeatureSpec extends fixture.FeatureSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  feature("Scope 1") {
    scenario("Test 1") { fixture => }
    scenario("Test 2") { fixture =>}
  }
  feature("Scope 2") {
    scenario("Test 3") { fixture => }
    scenario("Test 4") { fixture =>}
  }
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(2), "Scope 1 Scenario: Test 1")
    checkTestStarting(events(3), "Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(4), "Scope 1 Scenario: Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Scenario: Test 3")
    checkTestSucceeded(events(8), "Scope 2 Scenario: Test 3")
    checkTestStarting(events(9), "Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(10), "Scope 2 Scenario: Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFlatSpec extends FlatSpec with OrderExpectedResults with ParallelTestExecution {
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in {}
  it should "Test 4" in {}
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestSucceeded(events(8), "Scope 2 should Test 3")
    checkTestStarting(events(9), "Scope 2 should Test 4")
    checkTestSucceeded(events(10), "Scope 2 should Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFixtureFlatSpec extends fixture.FlatSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  behavior of "Scope 1"
  it should "Test 1" in { fixture => }
  it should "Test 2" in { fixture => }
  
  behavior of "Scope 2"
  it should "Test 3" in { fixture => }
  it should "Test 4" in { fixture => }
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestSucceeded(events(8), "Scope 2 should Test 3")
    checkTestStarting(events(9), "Scope 2 should Test 4")
    checkTestSucceeded(events(10), "Scope 2 should Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFreeSpec extends FreeSpec with OrderExpectedResults with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" - {
    "Test 3" in {}
    "Test 4" in {}
  }
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestSucceeded(events(8), "Scope 2 Test 3")
    checkTestStarting(events(9), "Scope 2 Test 4")
    checkTestSucceeded(events(10), "Scope 2 Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFixtureFreeSpec extends fixture.FreeSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  "Scope 1" - {
    "Test 1" in { fixture => }
    "Test 2" in { fixture => }
  }
  
  "Scope 2" - {
    "Test 3" in { fixture => }
    "Test 4" in { fixture => }
  }
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestSucceeded(events(8), "Scope 2 Test 3")
    checkTestStarting(events(9), "Scope 2 Test 4")
    checkTestSucceeded(events(10), "Scope 2 Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderPropSpec extends PropSpec with OrderExpectedResults with ParallelTestExecution {
  property("Test 1") {}
  property("Test 2") {}
  property("Test 3") {}
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestSucceeded(events(3), "Test 2")
    checkTestStarting(events(4), "Test 3")
    checkTestSucceeded(events(5), "Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFixturePropSpec extends fixture.PropSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  property("Test 1") { fixture => }
  property("Test 2") { fixture => }
  property("Test 3") { fixture => }
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestSucceeded(events(3), "Test 2")
    checkTestStarting(events(4), "Test 3")
    checkTestSucceeded(events(5), "Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderWordSpec extends WordSpec with OrderExpectedResults with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" should {
    "Test 3" in {}
    "Test 4" in {}
  }
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestSucceeded(events(8), "Scope 2 should Test 3")
    checkTestStarting(events(9), "Scope 2 should Test 4")
    checkTestSucceeded(events(10), "Scope 2 should Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionOrderFixtureWordSpec extends fixture.WordSpec with OrderExpectedResults with ParallelTestExecution with StringFixture {
  "Scope 1" should {
    "Test 1" in { fixture => }
    "Test 2" in { fixture => }
  }
  
  "Scope 2" should {
    "Test 3" in { fixture => }
    "Test 4" in { fixture => }
  }
  
  def assertOrderTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestSucceeded(events(8), "Scope 2 should Test 3")
    checkTestStarting(events(9), "Scope 2 should Test 4")
    checkTestSucceeded(events(10), "Scope 2 should Test 4")
    checkScopeClosed(events(11), "Scope 2")
  }
}
