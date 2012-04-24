/*
 * Copyright 2001-2011 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

import scala.collection.immutable.TreeSet
import org.scalatest.events._

class SuiteSuite extends Suite with PrivateMethodTester with SharedHelpers {

  def testNamesAndGroupsMethodsDiscovered() {

    val a = new Suite {
      def testNames(info: Informer): Unit = ()
    }
    assert(a.expectedTestCount(Filter()) === 1)
    val tnResult: Set[String] = a.testNames
    val gResult: Map[String, Set[String]] = a.tags
    assert(tnResult.size === 1)
    assert(gResult.keySet.size === 0)
  }

  def testThatTestMethodsWithNoGroupsDontShowUpInGroupsMap() {
    
    val a = new Suite {
      def testNotInAGroup() = ()
    }
    assert(a.tags.keySet.size === 0)
  }

  def testThatTestMethodsThatReturnNonUnitAreDiscovered() {
    val a = new Suite {
      def testThis(): Int = 1
      def testThat(info: Informer): String = "hi"
    }
    assert(a.expectedTestCount(Filter()) === 2)
    assert(a.testNames.size === 2)
    assert(a.tags.keySet.size === 0)
  }

  def testThatOverloadedTestMethodsAreDiscovered() {
    val a = new Suite {
      def testThis() = ()
      def testThis(info: Informer) = ()
    }
    assert(a.expectedTestCount(Filter()) === 2)
    assert(a.testNames.size === 2)
    assert(a.tags.keySet.size === 0)
  }

  def testThatInterceptCatchesSubtypes() {
    class MyException extends RuntimeException
    class MyExceptionSubClass extends MyException
    intercept[MyException] {
      throw new MyException
      new AnyRef // This is needed because right now Nothing doesn't overload as an Any
    }
    intercept[MyException] {
      throw new MyExceptionSubClass
      new AnyRef // This is needed because right now Nothing doesn't overload as an Any
    }
    // Try with a trait
    trait MyTrait {
      def someRandomMethod() {}
    }
    class AnotherException extends RuntimeException with MyTrait
    val caught = intercept[MyTrait] {
      throw new AnotherException
      new AnyRef // This is needed because right now Nothing doesn't overload as an Any
    }
    // Make sure the result type is the type passed in, so I can 
    // not cast and still invoke any method on it I want
    caught.someRandomMethod()
  }

  def testThatInterceptReturnsTheCaughtException() {
    val e = new RuntimeException
    val result = intercept[RuntimeException] {
      throw e
      new AnyRef // This is needed because right now Nothing doesn't overload as an Any
    }
    assert(result eq e)
  }

  def testStripDollars() {
    expect("MySuite") {
     Suite.stripDollars("line8$object$$iw$$iw$$iw$$iw$$iw$MySuite")
    }
    expect("MySuite") {
     Suite.stripDollars("MySuite")
    }
    expect("nested.MySuite") {
     Suite.stripDollars("nested.MySuite")
    }
    expect("$$$") {
     Suite.stripDollars("$$$") 
    }
    expect("DollarAtEnd") {
     Suite.stripDollars("DollarAtEnd$") 
    }
    expect("DollarAtEnd") {
     Suite.stripDollars("line8$object$$iw$$iw$$iw$$iw$$iw$DollarAtEnd$")
    }
    expect("MySuite$1") {
     Suite.stripDollars("MySuite$1")
    }
  }
  
  def testDiffStrings() {
    expect(("[]", "[a]")) { Suite.diffStrings("", "a") }
    expect(("[a]", "[]")) { Suite.diffStrings("a", "") }
    expect(("a[]", "a[b]")) { Suite.diffStrings("a", "ab") }
    expect(("a[b]", "a[]")) { Suite.diffStrings("ab", "a") }
    expect(("[a]", "[b]")) { Suite.diffStrings("a", "b") }
    expect(("[a]big", "[]big")) { Suite.diffStrings("abig", "big") }
    expect(("[]big", "[a]big")) { Suite.diffStrings("big", "abig") }
    expect(("big[a]", "big[]")) { Suite.diffStrings("biga", "big") }
    expect(("big[]", "big[a]")) { Suite.diffStrings("big", "biga") }
    expect(("small[a]big", "small[]big")) { Suite.diffStrings("smallabig", "smallbig") }
    expect(("0123456789[]0123456789", "0123456789[a]0123456789")) {
      Suite.diffStrings("01234567890123456789", "0123456789a0123456789")
    }
    expect(("...01234567890123456789[]0123456789", "...01234567890123456789[a]0123456789")) {
      Suite.diffStrings("X012345678901234567890123456789", "X01234567890123456789a0123456789")
    }
    expect(("01234567890123456789[]01234567890123456789...", "01234567890123456789[a]01234567890123456789...")) {
        Suite.diffStrings("0123456789012345678901234567890123456789X", "01234567890123456789a01234567890123456789X")
    }
    expect(("...01234567890123456789[]01234567890123456789...", "...01234567890123456789[a]01234567890123456789...")) {
        Suite.diffStrings("XXXX0123456789012345678901234567890123456789XX", "XXXX01234567890123456789a01234567890123456789XX")
    }
    expect(("...01234567890123456789[]01234567890123456789...", "...01234567890123456789[a]01234567890123456789...")) {
        Suite.diffStrings("X0123456789012345678901234567890123456789X", "X01234567890123456789a01234567890123456789X")
    }
  }

  def testDecorateToStringValue() {

    val decorateToStringValue = PrivateMethod[String]('decorateToStringValue)

    expect("1") {
      FailureMessages invokePrivate decorateToStringValue(1.toByte)
    }
    expect("1") {
      FailureMessages invokePrivate decorateToStringValue(1.toShort)
    }
    expect("1") {
      FailureMessages invokePrivate decorateToStringValue(1)
    }
    expect("10") {
      FailureMessages invokePrivate decorateToStringValue(10L)
    }
    expect("1.0") {
      FailureMessages invokePrivate decorateToStringValue(1.0f)
    }
    expect("1.0") {
      FailureMessages invokePrivate decorateToStringValue(1.0)
    }
    expect("false") {
      FailureMessages invokePrivate decorateToStringValue(false)
    }
    expect("true") {
      FailureMessages invokePrivate decorateToStringValue(true)
    }
    expect("<(), the Unit value>") {
      FailureMessages invokePrivate decorateToStringValue(())
    }
    expect("\"Howdy!\"") {
      FailureMessages invokePrivate decorateToStringValue("Howdy!")
    }
    expect("'c'") {
      FailureMessages invokePrivate decorateToStringValue('c')
    }
    expect("Hey!") {
      FailureMessages invokePrivate decorateToStringValue(new AnyRef { override def toString = "Hey!"})
    }
  }

  def testTestDurations() {

    class MySuite extends Suite {
      def testSucceeds() = ()
      def testFails() { fail() }
    }

    val mySuite = new MySuite
    val myReporter = new TestDurationReporter
    mySuite.run(None, myReporter, new Stopper {}, Filter(), Map(), None, new Tracker(new Ordinal(99)))
    assert(myReporter.testSucceededWasFiredAndHadADuration)
    assert(myReporter.testFailedWasFiredAndHadADuration)
  }

  def testSuiteDurations() {

    // the suite duration is sent by runNestedSuites, so MySuite needs a
    // nested suite
    class MySuite extends Suite {
      override def nestedSuites = List(new Suite {})
      def testSucceeds() = ()
      def testFails() { fail() }
    }

    val mySuite = new MySuite
    val myReporter = new SuiteDurationReporter
    mySuite.run(None, myReporter, new Stopper {}, Filter(), Map(), None, new Tracker(new Ordinal(99)))
    assert(myReporter.suiteCompletedWasFiredAndHadADuration)

    class SuiteThatAborts extends Suite {
      override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
              config: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
        throw new RuntimeException("Aborting for testing purposes")
      }
    }

    // the suite duration is sent by runNestedSuites, so MySuite needs a
    // nested suite
    class MyOtherSuite extends Suite {
      override def nestedSuites = List(new SuiteThatAborts)
      def testSucceeds() = ()
      def testFails() { fail() }
    }

    val myOtherSuite = new MyOtherSuite
    val myOtherReporter = new SuiteDurationReporter
    myOtherSuite.run(None, myOtherReporter, new Stopper {}, Filter(), Map(), None, new Tracker(new Ordinal(99)))
    assert(myOtherReporter.suiteAbortedWasFiredAndHadADuration)
  }

  def testPending() {

    class MySuite extends Suite {
      def testPending() { pending }
    }

    val mySuite = new MySuite
    val myReporter = new PendingReporter
    mySuite.run(None, myReporter, new Stopper {}, Filter(), Map(), None, new Tracker(new Ordinal(99)))
    assert(myReporter.testPendingWasFired)
  }

  def testPrettifyArray() {

    import FailureMessages.prettifyArrays

    // non arrays print just a toString
    assert(prettifyArrays(1) === "1")
    assert(prettifyArrays("hi") === "hi")
    assert(prettifyArrays(List(1, 2, 3)) === "List(1, 2, 3)")
    assert(prettifyArrays(Map("one" -> 1)) === "Map(one -> 1)")

    // arrays print pretty
    assert(prettifyArrays(Array(1, 2)) === "Array(1, 2)")

    // arrays of arrays print pretty
    assert(prettifyArrays(Array(Array(1, 2), Array(3, 4))) === "Array(Array(1, 2), Array(3, 4))")
  }

  def testExecute() {
    class TestWasCalledSuite extends Suite {
      var theTestThisCalled = false
      var theTestThatCalled = false
      var theTestThisConfigMapWasEmpty = true
      var theTestThatConfigMapWasEmpty = true
      override def withFixture(test: NoArgTest) {
        if (test.configMap.size > 0)
          test.name match {
            case "testThis" => theTestThisConfigMapWasEmpty = false
            case "testThat" => theTestThatConfigMapWasEmpty = false
            case _ => throw new Exception("Should never happen")
          }
        test()
      }
      def testThis() { theTestThisCalled = true }
      def testThat() { theTestThatCalled = true }
    }

    val s1 = new TestWasCalledSuite
    s1.execute()
    assert(s1.theTestThisCalled)
    assert(s1.theTestThatCalled)
    assert(s1.theTestThisConfigMapWasEmpty)
    assert(s1.theTestThatConfigMapWasEmpty)

    val s2 = new TestWasCalledSuite
    s2.execute("testThis")
    assert(s2.theTestThisCalled)
    assert(!s2.theTestThatCalled)
    assert(s2.theTestThisConfigMapWasEmpty)
    assert(s2.theTestThatConfigMapWasEmpty)

    val s3 = new TestWasCalledSuite
    s3.execute(configMap = Map("s" -> "s"))
    assert(s3.theTestThisCalled)
    assert(s3.theTestThatCalled)
    assert(!s3.theTestThisConfigMapWasEmpty)
    assert(!s3.theTestThatConfigMapWasEmpty)

    val s4 = new TestWasCalledSuite
    s4.execute("testThis", Map("s" -> "s"))
    assert(s4.theTestThisCalled)
    assert(!s4.theTestThatCalled)
    assert(!s4.theTestThisConfigMapWasEmpty)
    assert(s4.theTestThatConfigMapWasEmpty)

    val s5 = new TestWasCalledSuite
    s5.execute(testName = "testThis")
    assert(s5.theTestThisCalled)
    assert(!s5.theTestThatCalled)
    assert(s5.theTestThisConfigMapWasEmpty)
    assert(s5.theTestThatConfigMapWasEmpty)

    val s6 = new TestWasCalledSuite
    s6.execute(testName = "testThis", configMap = Map("s" -> "s"))
    assert(s6.theTestThisCalled)
    assert(!s6.theTestThatCalled)
    assert(!s6.theTestThisConfigMapWasEmpty)
    assert(s6.theTestThatConfigMapWasEmpty)
  }
  
  def testDecodedSuiteName() {
    expect("My Test") { new My$u0020Test().decodedSuiteName.get }
    expect(None) { new SuiteSuite().decodedSuiteName }
  }
  
  def testDecodedTestName() {
    
    class NormalSuite extends Suite {
      def testSucceed() = {}
      def testFail() = { fail }
      def testPending() = { pending }
      @Ignore
      def testIgnore() = {}
    }
    
    val normalSuite = new NormalSuite
    val normalReporter = new EventRecordingReporter
    normalSuite.run(None, normalReporter, new Stopper {}, Filter(), Map(), None, new Tracker(new Ordinal(99)))
    val normalEventList:List[Event] = normalReporter.eventsReceived
    expect(7) { normalEventList.size }
    normalEventList.foreach {event =>
      event match {
        case testStarting:TestStarting => 
          expect(None) { testStarting.decodedTestName }
          expect(None) { testStarting.decodedSuiteName }
        case testSucceed:TestSucceeded => 
          expect("testSucceed") { testSucceed.testName }
          expect(None) { testSucceed.decodedTestName }
        case testFail:TestFailed =>
          expect("testFail") { testFail.testName }
          expect(None) { testFail.decodedTestName }
        case testPending:TestPending =>
          expect("testPending") { testPending.testName }
          expect(None) { testPending.decodedTestName }
        case testIgnore:TestIgnored => 
          expect("testIgnore") { testIgnore.testName }
          expect(None) { testIgnore.decodedTestName }
        case _ =>
      }
    }
    
    class DecodedSuite extends Suite {
      def `test Succeed`() {}
      def `test Fail`() = { fail }
      def `test Pending`() = { pending }
      @Ignore
      def `test Ignore`() = {}
    }
    
    val decodedSuite = new DecodedSuite
    val decodedReporter = new EventRecordingReporter
    decodedSuite.run(None, decodedReporter, new Stopper {}, Filter(), Map(), None, new Tracker(new Ordinal(99)))
    val decodedEventList:List[Event] = decodedReporter.eventsReceived
    expect(7) { decodedEventList.size }
    decodedEventList.foreach {event =>
      event match {
        case testStarting:TestStarting => 
          testStarting.decodedTestName match {
            case Some(name) => assert(name.length() > 0, "decodedTestName should not be empty.")
            case None => fail("decodedTestName should not be empty.")
          }
          expect(None) { testStarting.decodedSuiteName }
        case testSucceed:TestSucceeded => 
          expect("test$u0020Succeed") { testSucceed.testName }
          expect(Some("test Succeed")) { testSucceed.decodedTestName }
        case testFail:TestFailed =>
          expect("test$u0020Fail") { testFail.testName }
          expect(Some("test Fail")) { testFail.decodedTestName }
        case testPending:TestPending =>
          expect("test$u0020Pending") { testPending.testName }
          expect(Some("test Pending")) { testPending.decodedTestName }
        case testIgnore:TestIgnored => 
          expect("test$u0020Ignore") { testIgnore.testName }
          expect(Some("test Ignore")) { testIgnore.decodedTestName }
        case _ =>
      }
    }
  }
  
  def testTestTags() {
    class TagSuite extends Suite {  
      def testNoTagMethod() {}
      @SlowAsMolasses
      def testTagMethod() {}
    }
    val testTags = new TagSuite().tags
    assert(testTags.size === 1)
    val tagSet = testTags.getOrElse("testTagMethod", null)
    assert(tagSet != null)
    assert(tagSet.size === 1)
    assert(tagSet.toList(0) === classOf[SlowAsMolasses].getName)
  }
  
  def testRunNestedSuite() {
    
    class NoTagSuite extends Suite
    @Ignore
    class IgnoreSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    @SlowAsMolasses
    class SlowAsMolassesSuite extends Suite
    @FastAsLight
    class FastAsLightSuite extends Suite
    
    class MasterSuite extends Suite {
      override def nestedSuites = List(new NoTagSuite(), new IgnoreSuite(), new SlowAsMolassesSuite(), new FastAsLightSuite())
      override def runNestedSuites(reporter: Reporter, stopper: Stopper, filter: Filter,
                                configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
        super.runNestedSuites(reporter, stopper, filter, configMap, distributor, tracker)
      }
    }
    
    class CounterDistributor extends Distributor {
      var count = 0
      def apply(suite: Suite, tracker: Tracker, filter: Filter) {
        count += 1
      }
      def apply(suite: Suite, tracker: Tracker) {
        throw new UnsupportedOperationException("ConcurrentDistributor does not support this operation, please use apply(suite: Suite, tracker: Tracker, filter: Filter) instead.")
      }
    }
    
    val masterSuite = new MasterSuite()
    
    val defaultFilter = new Filter(None, Set.empty)
    val defaultReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(defaultReporter, new Stopper {}, defaultFilter, Map.empty, None, new Tracker(new Ordinal(99)))
    assert(defaultReporter.suiteStartingEventsReceived.size === 4)
    assert(defaultReporter.testIgnoredEventsReceived.size === 3)
    val defaultReporterDist = new EventRecordingReporter
    val defaultDistributor = new CounterDistributor
    masterSuite.runNestedSuites(defaultReporterDist, new Stopper {}, defaultFilter, Map.empty, Some(defaultDistributor), new Tracker(new Ordinal(99)))
    assert(defaultDistributor.count === 4)
    
    val includeFilter = new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)
    val includeReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(includeReporter, new Stopper {}, includeFilter, Map.empty, None, new Tracker(new Ordinal(99)))
    assert(includeReporter.suiteStartingEventsReceived.size === 4) 
    assert(includeReporter.testIgnoredEventsReceived.size === 0) 
    val includeReporterDist = new EventRecordingReporter
    val includeDistributor = new CounterDistributor
    masterSuite.runNestedSuites(includeReporterDist, new Stopper {}, includeFilter, Map.empty, Some(includeDistributor), new Tracker(new Ordinal(99)))
    assert(includeDistributor.count === 4) 
    
    val excludeFilter = new Filter(None, Set("org.scalatest.SlowAsMolasses"))
    val excludeReporter = new EventRecordingReporter
    masterSuite.runNestedSuites(excludeReporter, new Stopper {}, excludeFilter, Map.empty, None, new Tracker(new Ordinal(99)))
    assert(excludeReporter.suiteStartingEventsReceived.size === 4)
    assert(excludeReporter.testIgnoredEventsReceived.size === 3)
    val excludeReporterDist = new EventRecordingReporter
    val excludeDistributor = new CounterDistributor
    masterSuite.runNestedSuites(excludeReporterDist, new Stopper {}, excludeFilter, Map.empty, Some(excludeDistributor), new Tracker(new Ordinal(99)))
    assert(excludeDistributor.count === 4)
  }
  
  def testExpectedTestCount() {
    class NoTagSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    @Ignore
    class IgnoreSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    @SlowAsMolasses
    class SlowAsMolassesSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    @FastAsLight
    class FastAsLightSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    
    class MasterSuite extends Suite {
      override def nestedSuites = List(new NoTagSuite(), new IgnoreSuite(), new SlowAsMolassesSuite(), new FastAsLightSuite())
      override def runNestedSuites(reporter: Reporter, stopper: Stopper, filter: Filter,
                                configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
        super.runNestedSuites(reporter, stopper, filter, configMap, distributor, tracker)
      }
    }
    
    val masterSuite = new MasterSuite()
    assert(masterSuite.expectedTestCount(new Filter(None, Set.empty)) === 9)
    assert(masterSuite.expectedTestCount(new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)) === 3)
    assert(masterSuite.expectedTestCount(new Filter(None, Set("org.scalatest.FastAsLight"))) === 6)
    assert(masterSuite.expectedTestCount(new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set.empty)) === 3)
    assert(masterSuite.expectedTestCount(new Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 6)
  }
  
  def testSuiteRunner() {
    assert(new NormalSuite().rerunner.get === classOf[NormalSuite].getName)
    assert(new WrappedSuite(Map.empty).rerunner.get === classOf[WrappedSuite].getName)
    assert(new NotAccessibleSuite("test").rerunner === None)
  }
  
  def testCheckChosenStyles() {
    class SimpleSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
    }
    
    val simpleSuite = new SimpleSuite()
    simpleSuite.checkChosenStyles(Map.empty)
    simpleSuite.checkChosenStyles(Map("org.scalatest.ChosenStyles" -> Set("Suite")))
    intercept[NotAllowedException] {
      simpleSuite.checkChosenStyles(Map("org.scalatest.ChosenStyles" -> Set("FunSpec")))
    }
  }
}

class `My Test` extends Suite {}
class NormalSuite extends Suite
@WrapWith(classOf[ConfigMapWrapperSuite]) 
class WrappedSuite(configMap: Map[_, _]) extends Suite
class NotAccessibleSuite(name: String) extends Suite
