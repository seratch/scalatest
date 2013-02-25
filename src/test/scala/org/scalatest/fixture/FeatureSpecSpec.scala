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
package org.scalatest.fixture

import org.scalatest._
import events.TestFailed
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestRegistrationClosedException

class FeatureSpecSpec extends org.scalatest.FunSpec with SharedHelpers {

  describe("A FeatureSpec") {
    it("should return the test names in order of registration from testNames") {
      val a = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        scenario("should do that") { fixture =>
        }
        scenario("should do this") { fixture =>
        }
      }

      assertResult(List("Scenario: should do that", "Scenario: should do this")) {
        a.testNames.iterator.toList
      }

      val b = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
      }

      assertResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        scenario("should do this") { fixture =>
        }
        scenario("should do that") { fixture =>
        }
      }

      assertResult(List("Scenario: should do this", "Scenario: should do that")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw NotAllowedException if a duplicate scenario name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          scenario("test this") { fixture =>
          }
          scenario("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          scenario("test this") { fixture =>
          }
          ignore("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("test this") { fixture =>
          }
          ignore("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("test this") { fixture =>
          }
          scenario("test this") { fixture =>
          }
        }
      }
    }

    it("should pass in the fixture to every test method") {
      val a = new FeatureSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }
        scenario("should do this") { fixture =>
          assert(fixture === hello)
        }
        scenario("should do that") { fixture =>
          assert(fixture === hello)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(!rep.eventsReceived.exists(_.isInstanceOf[TestFailed]))
    }
    it("should throw NullPointerException if a null test tag is provided") {
      // scenario
      intercept[NullPointerException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          scenario("hi", null) { fixture => }
        }
      }
      val caught = intercept[NullPointerException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          scenario("hi", mytags.SlowAsMolasses, null) { fixture => }
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          scenario("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => }
        }
      }
      // ignore
      intercept[NullPointerException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("hi", null) { fixture => }
        }
      }
      val caught2 = intercept[NullPointerException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("hi", mytags.SlowAsMolasses, null) { fixture => }
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => }
        }
      }
    }

    class TestWasCalledSuite extends FeatureSpec {
      type FixtureParam = String
      def withFixture(test: OneArgTest) { test("hi") }
      var theTestThisCalled = false
      var theTestThatCalled = false
      scenario("this") { fixture => theTestThisCalled = true }
      scenario("that") { fixture => theTestThatCalled = true }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("Scenario: this"), SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      val a = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        scenario("test this") { fixture => theTestThisCalled = true }
        scenario("test that") { fixture => theTestThatCalled = true }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, repA, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        scenario("test that") { fixture => theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, repB, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        scenario("test this") { fixture => theTestThisCalled = true }
        ignore("test that") { fixture => theTestThatCalled = true }
      }

      val repC = new TestIgnoredTrackingReporter
      c.run(None, repC, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test that", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      // The order I want is order of appearance in the file.
      // Will try and implement that tomorrow. Subtypes will be able to change the order.
      val d = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        ignore("test that") { fixture => theTestThatCalled = true }
      }

      val repD = new TestIgnoredTrackingReporter
      d.run(None, repD, new Stopper {}, Filter(), Map(), None, new Tracker)
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
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        scenario("test that") { fixture => theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("Scenario: test this"), repE, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        scenario("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        scenario("test that") { fixture => theTestThatCalled = true }
      }
      val repA = new TestIgnoredTrackingReporter
      a.run(None, repA, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        scenario("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        scenario("test that") { fixture => theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, repB, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), Map(), None, new Tracker)
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        scenario("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        scenario("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, repB, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), Map(), None, new Tracker)
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        scenario("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, repD, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), Map(), None, new Tracker)
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        scenario("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        scenario("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, repE, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker)
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        scenario("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        scenario("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, repF, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker)
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        scenario("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        ignore("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, repG, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker)
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        scenario("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        scenario("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, repH, new Stopper {}, Filter(None, Set("org.scalatest.FastAsLight")), Map(), None, new Tracker)
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        scenario("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        scenario("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        scenario("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, repI, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses")), Map(), None, new Tracker)
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        scenario("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, repJ, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses")), Map(), None, new Tracker)
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        ignore("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repK = new TestIgnoredTrackingReporter
      k.run(None, repK, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), Map(), None, new Tracker)
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        scenario("test this") { fixture => }
        scenario("test that") { fixture => }
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        ignore("test this") { fixture => }
        scenario("test that") { fixture => }
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        scenario("test this", mytags.FastAsLight) { fixture => }
        scenario("test that") { fixture => }
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        scenario("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => }
        scenario("test that", mytags.SlowAsMolasses) { fixture => }
        scenario("test the other thing") { fixture => }
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        scenario("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => }
        scenario("test that", mytags.SlowAsMolasses) { fixture => }
        ignore("test the other thing") { fixture => }
      }
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(e.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(e.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 0)
      assert(e.expectedTestCount(Filter()) === 2)

      val f = new Suites(a, b, c, d, e)
      assert(f.expectedTestCount(Filter()) === 10)
    }

    it("should generate a TestPending message when the test body is (pending)") {
      val a = new FeatureSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }

        scenario("should do this") (pending)

        scenario("should do that") { fixture =>
          assert(fixture === hello)
        }
        scenario("should do something else") { fixture =>
          assert(fixture === hello)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new FeatureSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }
        scenario("throws AssertionError") { s => throw new AssertionError }
        scenario("throws plain old Error") { s => throw new Error }
        scenario("throws Throwable") { s => throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FeatureSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }
        scenario("throws AssertionError") { s => throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to true for info " +
            "calls made from a test that is pending") {
      val a = new FeatureSpec with GivenWhenThen {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }
        scenario("should do something else") { s =>
          Given("two integers")
          When("one is subracted from the other")
          Then("the result is the difference between the two numbers")
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      val ip = rep.infoProvidedEventsReceived
      assert(ip.size === 3)
      for (event <- ip) {
        assert(event.aboutAPendingTest.isDefined && event.aboutAPendingTest.get)
      }
    }
    it("should send InfoProvided events with aboutAPendingTest set to false for info " +
            "calls made from a test that is not pending") {
      val a = new FeatureSpec with GivenWhenThen {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }
        scenario("should do something else") { s =>
          Given("two integers")
          When("one is subracted from the other")
          Then("the result is the difference between the two numbers")
          assert(1 + 1 === 2)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
      val ip = rep.infoProvidedEventsReceived
      assert(ip.size === 3)
      for (event <- ip) {
        assert(event.aboutAPendingTest.isDefined && !event.aboutAPendingTest.get)
      }
    }
    it("should allow both tests that take fixtures and tests that don't") {
      val a = new FeatureSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest) {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        scenario("take no args") { () =>
          takesNoArgsInvoked = true
        }

        var takesAFixtureInvoked = false
        scenario("takes a fixture") { s => takesAFixtureInvoked = true }
      }

      a.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }
    it("should work with test functions whose inferred result type is not Unit") {
      val a = new FeatureSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest) {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        scenario("should take no args") { () =>
          takesNoArgsInvoked = true; true
        }

        var takesAFixtureInvoked = false
        scenario("should take a fixture") { s => takesAFixtureInvoked = true; true }
      }

      assert(!a.takesNoArgsInvoked)
      assert(!a.takesAFixtureInvoked)
      a.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }
    it("should work with ignored tests whose inferred result type is not Unit") {
      val a = new FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("should test this") { () =>
          theTestThisCalled = true; "hi"
        }
        ignore("should test that") { fixture => theTestThatCalled = true; 42 }
      }

      assert(!a.theTestThisCalled)
      assert(!a.theTestThatCalled)
      val reporter = new EventRecordingReporter
      a.run(None, reporter, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(reporter.testIgnoredEventsReceived.size === 2)
      assert(!a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }
    it("should pass a NoArgTest to withFixture for tests that take no fixture") {
      class MySpec extends FeatureSpec {
        type FixtureParam = String
        var aNoArgTestWasPassed = false
        var aOneArgTestWasPassed = false
        override def withFixture(test: NoArgTest) {
          aNoArgTestWasPassed = true
        }
        def withFixture(test: OneArgTest) {
          aOneArgTestWasPassed = true
        }
        scenario("something") { () =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySpec
      s.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(s.aNoArgTestWasPassed)
      assert(!s.aOneArgTestWasPassed)
    }
    it("should not pass a NoArgTest to withFixture for tests that take a Fixture") {
      class MySpec extends FeatureSpec {
        type FixtureParam = String
        var aNoArgTestWasPassed = false
        var aOneArgTestWasPassed = false
        override def withFixture(test: NoArgTest) {
          aNoArgTestWasPassed = true
        }
        def withFixture(test: OneArgTest) {
          aOneArgTestWasPassed = true
        }
        scenario("something") { fixture =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySpec
      s.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(!s.aNoArgTestWasPassed)
      assert(s.aOneArgTestWasPassed)
    }
    it("should pass a NoArgTest that invokes the no-arg test when the " +
            "NoArgTest's no-arg apply method is invoked") {

      class MySuite extends FeatureSpec {
        type FixtureParam = String
        var theNoArgTestWasInvoked = false
        def withFixture(test: OneArgTest) {
          // Shouldn't be called, but just in case don't invoke a OneArgTest
        }
        scenario("something") { () =>
          theNoArgTestWasInvoked = true
        }
      }

      val s = new MySuite
      s.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(s.theNoArgTestWasInvoked)
    }
    describe("(when a nesting rule has been violated)") {

      it("should, if they call a feature from within an scenario clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          scenario("should blow up") { fixture =>
            feature("in the wrong place, at the wrong time") {
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a feature with a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          scenario("should blow up") { fixture =>
            feature("in the wrong place, at the wrong time") {
              scenario("should never run") { fixture =>
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
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          scenario("should blow up") { fixture =>
            scenario("should never run") { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          scenario("should blow up") { fixture =>
            scenario("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a feature with a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          scenario("should blow up") { fixture =>
            feature("in the wrong place, at the wrong time") {
              ignore("should never run") { fixture =>
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
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          scenario("should blow up") { fixture =>
            ignore("should never run") { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySpec extends FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          scenario("should blow up") { fixture =>
            ignore("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySpec
        ensureTestFailedEventReceived(spec, "Scenario: should blow up")
      }
      it("should, if they call a nested feature from within a feature clause, result in a SuiteAborted event when constructing the FeatureSpec") {

        class MySpec extends FeatureSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          feature("should blow up") {
            feature("should never run") {
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
  }
  it("should pass the correct test name in the OneArgTest passed to withFixture") {
    val a = new FeatureSpec {
      type FixtureParam = String
      var correctTestNameWasPassed = false
      def withFixture(test: OneArgTest) {
        correctTestNameWasPassed = test.name == "Scenario: should do something"
        test("hi")
      }
      scenario("should do something") { fixture => }
    }
    a.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
    assert(a.correctTestNameWasPassed)
  }
  it("should pass the correct config map in the OneArgTest passed to withFixture") {
    val a = new FeatureSpec {
      type FixtureParam = String
      var correctConfigMapWasPassed = false
      def withFixture(test: OneArgTest) {
        correctConfigMapWasPassed = (test.configMap == Map("hi" -> 7))
        test("hi")
      }
      scenario("should do something") { fixture => }
    }
    a.run(None, SilentReporter, new Stopper {}, Filter(), Map("hi" -> 7), None, new Tracker())
    assert(a.correctConfigMapWasPassed)
  }
  
  describe("when failure happens") {
      
    it("should fire TestFailed event with correct stack depth info when test failed") {
      class TestSpec extends FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("a string") }
        scenario("fail scenario") { fixture =>
          assert(1 === 2)
        }
        feature("a feature") {
          scenario("nested fail scenario") { fixture =>
            assert(1 === 2)
          }
        }
      }
      val rep = new EventRecordingReporter
      val s1 = new TestSpec
      s1.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker)
      assert(rep.testFailedEventsReceived.size === 2)
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FeatureSpecSpec.scala")
      assert(rep.testFailedEventsReceived(0).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 13)
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeFileName.get === "FeatureSpecSpec.scala")
      assert(rep.testFailedEventsReceived(1).throwable.get.asInstanceOf[TestFailedException].failedCodeLineNumber.get === thisLineNumber - 11)
    }
      
    it("should generate NotAllowedException with correct stack depth info when has a feature nested inside a feature") {
      class TestSpec extends FeatureSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("a string") }
        feature("a feature") {
          feature("inner feature") {
            ignore("nested fail scenario") { fixture => 
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
    
    it("should generate TestRegistrationClosedException with correct stack depth info when has an scenario nested inside a scenario") {
        class TestSpec extends FeatureSpec {
          type FixtureParam = String
          var registrationClosedThrown = false
          feature("a feature") {
            scenario("a scenario") { fixture => 
              scenario("nested scenario") { fixture => 
                assert(1 === 2)
              }
            }
          }
          def withFixture(test: OneArgTest) {
            try {
              test("a string")
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
        s.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker)
        assert(s.registrationClosedThrown == true)
        val testFailedEvents = rep.testFailedEventsReceived
        assert(testFailedEvents.size === 1)
        assert(testFailedEvents(0).throwable.get.getClass() === classOf[TestRegistrationClosedException])
        val trce = testFailedEvents(0).throwable.get.asInstanceOf[TestRegistrationClosedException]
        assert("FeatureSpecSpec.scala" === trce.failedCodeFileName.get)
        assert(trce.failedCodeLineNumber.get === thisLineNumber - 25)
      }
  }
  
}
