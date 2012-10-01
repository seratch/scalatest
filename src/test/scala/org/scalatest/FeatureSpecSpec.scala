/*
 * Copyright 2001-2009 Artima, Inc.
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

import org.scalatest.events.TestStarting
import org.scalatest.events.InfoProvided
import org.scalatest.events.MarkupProvided
/* Uncomment once remove deprecated type aliases in org.scalatest
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestFailedException
*/

class FeatureSpecSpec extends FunSpec with SharedHelpers with EventHelpers {

  describe("A FeatureSpec") {

    it("should return the scenario names in registration order from testNames") {

      val a = new FeatureSpec {
        Scenario("test this") {}
        Scenario("test that") {}
      }

      expectResult(List("Scenario: test this", "Scenario: test that")) {
        a.testNames.iterator.toList
      }

      val b = new FeatureSpec {}

      expectResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FeatureSpec {
        Scenario("test that") {}
        Scenario("test this") {}
      }

      expectResult(List("Scenario: test that", "Scenario: test this")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw NotAllowedException if a duplicate scenario name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new FeatureSpec {
          Scenario("test this") {}
          Scenario("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FeatureSpec {
          Scenario("test this") {}
          ignore("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FeatureSpec {
          ignore("test this") {}
          ignore("test this") {}
        }
      }
      intercept[DuplicateTestNameException] {
        new FeatureSpec {
          ignore("test this") {}
          Scenario("test this") {}
        }
      }
    }

    it("should run tests registered via the scenariosFor syntax") {
      trait SharedFeatureSpecTests { this: FeatureSpec =>
        def nonEmptyStack(s: String)(i: Int) {
          Scenario("I am shared") {}
        }
      }
      class MySuite extends FeatureSpec with SharedFeatureSpecTests {
        scenariosFor(nonEmptyStack("hi")(1))
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val indexedList = reporter.eventsReceived

      val testStartingOption = indexedList.find(_.isInstanceOf[TestStarting])
      assert(testStartingOption.isDefined)
      assert(testStartingOption.get.asInstanceOf[TestStarting].testName === "Scenario: I am shared")
    }

    it("should throw NullPointerException if a null test tag is provided") {
      // scenario
      intercept[NullPointerException] {
        new FeatureSpec {
          Scenario("hi", null) {}
        }
      }
      val caught = intercept[NullPointerException] {
        new FeatureSpec {
          Scenario("hi", mytags.SlowAsMolasses, null) {}
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FeatureSpec {
          Scenario("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {}
        }
      }
      // ignore
      intercept[NullPointerException] {
        new FeatureSpec {
          ignore("hi", null) {}
        }
      }
      val caught2 = intercept[NullPointerException] {
        new FeatureSpec {
          ignore("hi", mytags.SlowAsMolasses, null) {}
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FeatureSpec {
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) {}
        }
      }
    }

    class TestWasCalledSuite extends FeatureSpec {
      var theTestThisCalled = false
      var theTestThatCalled = false
      Scenario("this") { theTestThisCalled = true }
      Scenario("that") { theTestThatCalled = true }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("Scenario: this"), Args(SilentReporter))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      val a = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this") { theTestThisCalled = true }
        Scenario("test that") { theTestThatCalled = true }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        Scenario("test that") { theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this") { theTestThisCalled = true }
        ignore("test that") { theTestThatCalled = true }
      }

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test that", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      // The order I want is order of appearance in the file.
      // Will try and implement that tomorrow. Subtypes will be able to change the order.
      val d = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        ignore("test that") { theTestThatCalled = true }
      }

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName endsWith "test that") // last because should be in order of appearance
      assert(!d.theTestThisCalled)
      assert(!d.theTestThatCalled)
    }

    it("should ignore a test marked as ignored if run is invoked with that testName") {
      // If I provide a specific testName to run, then it should ignore an Ignore on that test
      // method and actually invoke it.
      val e = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { theTestThisCalled = true }
        Scenario("test that") { theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("Scenario: test this"), Args(repE))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        Scenario("test that") { theTestThatCalled = true }
      }
      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        Scenario("test that") { theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), Map(), None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        Scenario("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), Map(), None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { theTestThisCalled = true }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), Map(), None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        Scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        Scenario("test the other") { theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        Scenario("test the other") { theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        Scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        ignore("test the other") { theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, Stopper.default, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        Scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        Scenario("test the other") { theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, Stopper.default, Filter(None, Set("org.scalatest.FastAsLight")), Map(), None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        Scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        Scenario("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        Scenario("test the other") { theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), Map(), None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        Scenario("test the other") { theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses")), Map(), None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FeatureSpec {
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { theTestThatCalled = true }
        ignore("test the other") { theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, Args(repK, Stopper.default, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), Map(), None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }
    
    it("should return the correct test count from its expectedTestCount method") {

      val a = new FeatureSpec {
        Scenario("test this") {}
        Scenario("test that") {}
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FeatureSpec {
        ignore("test this") {}
        Scenario("test that") {}
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FeatureSpec {
        Scenario("test this", mytags.FastAsLight) {}
        Scenario("test that") {}
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FeatureSpec {
        Scenario("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {}
        Scenario("test that", mytags.SlowAsMolasses) {}
        Scenario("test the other thing") {}
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FeatureSpec {
        Scenario("test this", mytags.FastAsLight, mytags.SlowAsMolasses) {}
        Scenario("test that", mytags.SlowAsMolasses) {}
        ignore("test the other thing") {}
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }
    
    it("should send an InfoProvided event for an info") {
      class MySuite extends FeatureSpec  {
        info(
          "hi there"
        )
      }
      val suite = new MySuite
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter))

      val infoList = reporter.infoProvidedEventsReceived

      assert(infoList.size === 1)
      assert(infoList(0).message === "hi there")
    }
    it("should generate a TestPending message when the test body is (pending)") {
      val a = new FeatureSpec {

        Scenario("should do this") (pending)

        Scenario("should do that") {
          assert(2 + 2 === 4)
        }
        
        Scenario("should do something else") {
          assert(2 + 2 === 4)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new FeatureSpec {
        Scenario("throws AssertionError") { throw new AssertionError }
        Scenario("throws plain old Error") { throw new Error }
        Scenario("throws Throwable") { throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FeatureSpec {
        Scenario("throws AssertionError") { throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter))
      }
    }
/*
    it("should send InfoProvided events with aboutAPendingTest set to true and aboutACanceledTest set to false for info " +
            "calls made from a test that is pending") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testPending = rep.testPendingEventsReceived
      assert(testPending.size === 1)
      val recordedEvents = testPending(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && ip.aboutAPendingTest.get)
        assert(ip.aboutACanceledTest.isDefined && !ip.aboutACanceledTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest and aboutACanceledTest set to false for info " +
            "calls made from a test that is not pending or canceled") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          assert(1 + 1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testSucceeded = rep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      val recordedEvents = testSucceeded(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && !ip.aboutAPendingTest.get)
        assert(ip.aboutACanceledTest.isDefined && !ip.aboutACanceledTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false and aboutACanceledTest set to true for info " +
            "calls made from a test that is canceled") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          given("two integers")
          when("one is subracted from the other")
          then("the result is the difference between the two numbers")
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testCanceled = rep.testCanceledEventsReceived
      assert(testCanceled.size === 1)
      val recordedEvents = testCanceled(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val ip = event.asInstanceOf[InfoProvided]
        assert(ip.aboutAPendingTest.isDefined && !ip.aboutAPendingTest.get)
        assert(ip.aboutACanceledTest.isDefined && ip.aboutACanceledTest.get)
      }
    }
    it("should send MarkupProvided events with aboutAPendingTest set to true and aboutACanceledTest set to false for markup " +
            "calls made from a test that is pending") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          markup("two strings")
          markup("walked into")
          markup("a bar")
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testPending = rep.testPendingEventsReceived
      assert(testPending.size === 1)
      val recordedEvents = testPending(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val mp = event.asInstanceOf[MarkupProvided]
        assert(mp.aboutAPendingTest.isDefined && mp.aboutAPendingTest.get)
        assert(mp.aboutACanceledTest.isDefined && !mp.aboutACanceledTest.get)
      }
    }
    it("should send MarkupProvided events with aboutAPendingTest and aboutACanceledTest set to false for markup " +
            "calls made from a test that is not pending or canceled") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          markup("two strings")
          markup("walked into")
          markup("a bar")
          assert(1 + 1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testSucceeded = rep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      val recordedEvents = testSucceeded(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val mp = event.asInstanceOf[MarkupProvided]
        assert(mp.aboutAPendingTest.isDefined && !mp.aboutAPendingTest.get)
        assert(mp.aboutACanceledTest.isDefined && !mp.aboutACanceledTest.get)
      }
    }
    it("should send MarkupProvided events with aboutAPendingTest set to false and aboutACanceledTest set to true for markup " +
            "calls made from a test that is canceled") {
      val a = new FeatureSpec with GivenWhenThen {
        scenario("should do something else") {
          markup("two strings")
          markup("walked into")
          markup("a bar")
          cancel()
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep))
      val testCanceled = rep.testCanceledEventsReceived
      assert(testCanceled.size === 1)
      val recordedEvents = testCanceled(0).recordedEvents
      assert(recordedEvents.size === 3)
      for (event <- recordedEvents) {
        val mp = event.asInstanceOf[MarkupProvided]
        assert(mp.aboutAPendingTest.isDefined && !mp.aboutAPendingTest.get)
        assert(mp.aboutACanceledTest.isDefined && mp.aboutACanceledTest.get)
      }
    }
*/
    it("should invoke withFixture from runTest") {
      val a = new FeatureSpec {
        var withFixtureWasInvoked = false
        var testWasInvoked = false
        override def withFixture(test: NoArgTest) {
          withFixtureWasInvoked = true
          super.withFixture(test)
        }
        Scenario("something") {
          testWasInvoked = true
        }
      }
      a.run(None, Args(SilentReporter))
      assert(a.withFixtureWasInvoked)
      assert(a.testWasInvoked)
    }
    it("should pass the correct test name in the NoArgTest passed to withFixture") {
      val a = new FeatureSpec {
        var correctTestNameWasPassed = false
        override def withFixture(test: NoArgTest) {
          correctTestNameWasPassed = test.name == "Scenario: should do something"
          super.withFixture(test)
        }
        Scenario("should do something") {}
      }
      a.run(None, Args(SilentReporter))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the NoArgTest passed to withFixture") {
      val a = new FeatureSpec {
        var correctConfigMapWasPassed = false
        override def withFixture(test: NoArgTest) {
          correctConfigMapWasPassed = (test.configMap == Map("hi" -> 7))
          super.withFixture(test)
        }
        Scenario("should do something") {}
      }
      a.run(None, Args(SilentReporter, Stopper.default, Filter(), Map("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a feature from within an scenario clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          Scenario("should blow up") {
            Feature("in the wrong place, at the wrong time") {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a feature with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          Scenario("should blow up") {
            Feature("in the wrong place, at the wrong time") {
              Scenario("should never run") {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          Scenario("should blow up") {
            Scenario("should never run") {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          Scenario("should blow up") {
            Scenario("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a feature with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          Scenario("should blow up") {
            Feature("in the wrong place, at the wrong time") {
              ignore("should never run") {
                assert(1 === 1)
              }
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          Scenario("should blow up") {
            ignore("should never run") {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          Scenario("should blow up") {
            ignore("should never run", mytags.SlowAsMolasses) {
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested feature from within a feature clause, result in a SuiteAborted event when constructing the FeatureSpec") {

        class MySpec extends FeatureSpec {
          Feature("should blow up") {
            Feature("should never run") {
            }
          }
        }

        val caught =
          intercept[NotAllowedException] {
            new MySpec
          }
        assert(caught.getMessage === "Feature clauses cannot be nested.")
      }
    }
    
    class ExamplePrefixSpec extends FeatureSpec {
      Feature("A Feature") {
        Scenario("A Scenario") {
          
        }
      }
    }
    
    it("should prefix feature text with 'Feature: '") {
      val rep = new EventRecordingReporter
      (new ExamplePrefixSpec).run(None, Args(rep))
      val scopeOpened = rep.scopeOpenedEventsReceived
      assert(scopeOpened.size === 1)
      assert(scopeOpened(0).message === "Feature: A Feature")
      val scopeClosed = rep.scopeClosedEventsReceived
      assert(scopeClosed.size === 1)
      assert(scopeClosed(0).message === "Feature: A Feature")
    }
    
    it("should prefix scenario text with 'Scenario: '") {
      val rep = new EventRecordingReporter
      (new ExamplePrefixSpec).run(None, Args(rep))
      val testStarting = rep.testStartingEventsReceived
      assert(testStarting.size === 1)
      assert(testStarting(0).testText === "Scenario: A Scenario")
      val testSucceeded = rep.testSucceededEventsReceived
      assert(testSucceeded.size === 1)
      assert(testSucceeded(0).testText === "Scenario: A Scenario")
    }
    
    describe("when failure happens") {
      
      it("should fire TestFailed event with correct stack depth info when test failed") {
        class TestSpec extends FeatureSpec {
          Scenario("fail scenario") {
            assert(1 === 2)
          }
          Feature("a feature") {
            Scenario("nested fail scenario") {
              assert(1 === 2)
            }
          }
        }
        val rep = new EventRecordingReporter
        val s1 = new TestSpec
        s1.run(None, Args(rep))
        assert(rep.testFailedEventsReceived.size === 2)
        assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FeatureSpecSpec.scala")
        assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 13)
        assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FeatureSpecSpec.scala")
        assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
      }
      
      it("should generate NotAllowedException with correct stack depth info when has a feature nested inside a feature") {
        class TestSpec extends FeatureSpec {
          Feature("a feature") {
            Feature("inner feature") {
              ignore("nested fail scenario") {
                assert(1 === 1)
              }
            }
          }
        }
        val rep = new EventRecordingReporter
        val caught = intercept[NotAllowedException] {
          new TestSpec
        }
        assert(caught.failedCodeFileName.get === "FeatureSpecSpec.scala")
        assert(caught.failedCodeLineNumber.get === thisLineNumber - 12)
      }
      
      it("should generate TestRegistrationClosedException with correct stack depth info when has a scenario nested inside a scenario") {
        class TestSpec extends FeatureSpec {
          var registrationClosedThrown = false
          Feature("a feature") {
            Scenario("a scenario") {
              Scenario("nested scenario") {
                assert(1 === 2)
              }
            }
          }
          override def withFixture(test: NoArgTest) {
            try {
              test.apply()
            }
            catch {
              case e: TestRegistrationClosedException => 
                registrationClosedThrown = true
                throw e
            }
          }
        }
        val rep = new EventRecordingReporter
        val s = new TestSpec
        s.run(None, Args(rep))
        assert(s.registrationClosedThrown == true)
        val testFailedEvents = rep.testFailedEventsReceived
        assert(testFailedEvents.size === 1)
        assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
        val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
        assert("FeatureSpecSpec.scala" === trce.failedCodeFileName.get)
        assert(trce.failedCodeLineNumber.get === thisLineNumber - 25)
      }
    }
    
    it("should send out justifications as info") {
      class TestSpec extends FeatureSpec {
        Justification(
          "As a programmer",
          "I want to be able to pop items off the stack",
          "So that I can get them in last-in-first-out order"
        )

        Scenario("pop is invoked on a non-empty stack") {}
        Scenario("pop is invoked on an empty stack") {}
      }
      
      val rep = new EventRecordingReporter
      val s = new TestSpec
      s.run(None, Args(rep))
      val events = rep.eventsReceived
      assert(rep.eventsReceived.length === 7)
      checkInfoProvided(events(0), "As a programmer")
      checkInfoProvided(events(1), "I want to be able to pop items off the stack")
      checkInfoProvided(events(2), "So that I can get them in last-in-first-out order")
      checkTestStarting(events(3), "Scenario: pop is invoked on a non-empty stack")
      checkTestSucceeded(events(4), "Scenario: pop is invoked on a non-empty stack")
      checkTestStarting(events(5), "Scenario: pop is invoked on an empty stack")
      checkTestSucceeded(events(6), "Scenario: pop is invoked on an empty stack")
    }
    
    it("should allow justifications in Feature") {
      class TestSpec extends FeatureSpec {
        Feature("Stack") {
          Justification(
            "As a programmer",
            "I want to be able to pop items off the stack",
            "So that I can get them in last-in-first-out order"
          )

          Scenario("pop is invoked on a non-empty stack") {}
          Scenario("pop is invoked on an empty stack") {}
        }
      }
      
      val rep = new EventRecordingReporter
      val s = new TestSpec
      s.run(None, Args(rep))
      val events = rep.eventsReceived
      assert(rep.eventsReceived.length === 9)
      checkScopeOpened(events(0), "Feature: Stack")
      checkInfoProvided(events(1), "As a programmer")
      checkInfoProvided(events(2), "I want to be able to pop items off the stack")
      checkInfoProvided(events(3), "So that I can get them in last-in-first-out order")
      checkTestStarting(events(4), "Feature: Stack Scenario: pop is invoked on a non-empty stack")
      checkTestSucceeded(events(5), "Feature: Stack Scenario: pop is invoked on a non-empty stack")
      checkTestStarting(events(6), "Feature: Stack Scenario: pop is invoked on an empty stack")
      checkTestSucceeded(events(7), "Feature: Stack Scenario: pop is invoked on an empty stack")
      checkScopeClosed(events(8), "Feature: Stack")
    }
    
    it("should throw NotAllowedException with correct stack depth when justification does not come first") {
      class TestSpec extends FeatureSpec {
        Scenario("pop is invoked on a non-empty stack") {}
        
        Justification(
          "As a programmer",
          "I want to be able to pop items off the stack",
          "So that I can get them in last-in-first-out order"
        )
        
        Scenario("pop is invoked on an empty stack") {}
      }
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("FeatureSpecSpec.scala" === e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get === thisLineNumber - 12)
    }
    
    it("should also throw NotAllowedException with correct stack depth when justification does not come first in Feature block") {
      class TestSpec extends FeatureSpec {
        Feature("Stack") {
          Scenario("pop is invoked on a non-empty stack") {}
          Justification(
            "As a programmer",
            "I want to be able to pop items off the stack",
            "So that I can get them in last-in-first-out order"
          )
          Scenario("pop is invoked on an empty stack") {}
        }
      }
      
      val e = intercept[NotAllowedException] {
        new TestSpec
      }
      assert("FeatureSpecSpec.scala" === e.failedCodeFileName.get)
      assert(e.failedCodeLineNumber.get === thisLineNumber - 13)
    }
  }
}
