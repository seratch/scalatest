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

class FixturePropSpecSpec extends org.scalatest.FunSpec with PrivateMethodTester with SharedHelpers {

  describe("A FixturePropSpec") {
    it("should return the test names in order of registration from testNames") {
      val a = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        property("that") { fixture =>
        }
        property("this") { fixture =>
        }
      }

      expectResult(List("that", "this")) {
        a.testNames.iterator.toList
      }

      val b = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
      }

      expectResult(List[String]()) {
        b.testNames.iterator.toList
      }

      val c = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        property("this") { fixture =>
        }
        property("that") { fixture =>
        }
      }

      expectResult(List("this", "that")) {
        c.testNames.iterator.toList
      }
    }

    it("should throw NotAllowedException if a duplicate test name registration is attempted") {

      intercept[DuplicateTestNameException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          property("test this") { fixture =>
          }
          property("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          property("test this") { fixture =>
          }
          ignore("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("test this") { fixture =>
          }
          ignore("test this") { fixture =>
          }
        }
      }
      intercept[DuplicateTestNameException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("test this") { fixture =>
          }
          property("test this") { fixture =>
          }
        }
      }
    }

    it("should pass in the fixture to every test method") {
      val a = new FixturePropSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }
        property("this") { fixture =>
          assert(fixture === hello)
        }
        property("that") { fixture =>
          assert(fixture === hello)
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      assert(!rep.eventsReceived.exists(_.isInstanceOf[TestFailed]))
    }

    it("should throw NullPointerException if a null test tag is provided") {
      // test
      intercept[NullPointerException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          property("hi", null) { fixture => }
        }
      }
      val caught = intercept[NullPointerException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          property("hi", mytags.SlowAsMolasses, null) { fixture => }
        }
      }
      assert(caught.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          property("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => }
        }
      }
      // ignore
      intercept[NullPointerException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("hi", null) { fixture => }
        }
      }
      val caught2 = intercept[NullPointerException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("hi", mytags.SlowAsMolasses, null) { fixture => }
        }
      }
      assert(caught2.getMessage === "a test tag was null")
      intercept[NullPointerException] {
        new FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) {}
          ignore("hi", mytags.SlowAsMolasses, null, mytags.WeakAsAKitten) { fixture => }
        }
      }
    }
    it("should return a correct tags map from the tags method") {

      val a = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        ignore("test this") { fixture => }
        property("test that") { fixture => }
      }
      expectResult(Map("test this" -> Set("org.scalatest.Ignore"))) {
        a.tags
      }

      val b = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        property("test this") { fixture => }
        ignore("test that") { fixture => }
      }
      expectResult(Map("test that" -> Set("org.scalatest.Ignore"))) {
        b.tags
      }

      val c = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        ignore("test this") { fixture => }
        ignore("test that") { fixture => }
      }
      expectResult(Map("test this" -> Set("org.scalatest.Ignore"), "test that" -> Set("org.scalatest.Ignore"))) {
        c.tags
      }

      val d = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        property("test this", mytags.SlowAsMolasses) { fixture => }
        ignore("test that", mytags.SlowAsMolasses) { fixture => }
      }
      expectResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses"), "test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
        d.tags
      }

      val e = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
      }
      expectResult(Map()) {
        e.tags
      }

      val f = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {}
        property("test this", mytags.SlowAsMolasses, mytags.WeakAsAKitten) { fixture => }
        property("test that", mytags.SlowAsMolasses) { fixture => }
      }
      expectResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
        f.tags
      }
    }
    
    class TestWasCalledSuite extends FixturePropSpec {
      type FixtureParam = String
      def withFixture(test: OneArgTest) { test("hi") }
      var theTestThisCalled = false
      var theTestThatCalled = false
      property("this") { fixture => theTestThisCalled = true }
      property("that") { fixture => theTestThatCalled = true }
    }

    it("should execute all tests when run is called with testName None") {

      val b = new TestWasCalledSuite
      b.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(b.theTestThisCalled)
      assert(b.theTestThatCalled)
    }

    it("should execute one test when run is called with a defined testName") {

      val a = new TestWasCalledSuite
      a.run(Some("this"), Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }

    it("should report as ignored, and not run, tests marked ignored") {

      val a = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this") { fixture => theTestThisCalled = true }
        property("test that") { fixture => theTestThatCalled = true }
      }

      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      val b = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        property("test that") { fixture => theTestThatCalled = true }
      }

      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(repB.testIgnoredReceived)
      assert(repB.lastEvent.isDefined)
      assert(repB.lastEvent.get.testName endsWith "test this")
      assert(!b.theTestThisCalled)
      assert(b.theTestThatCalled)

      val c = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this") { fixture => theTestThisCalled = true }
        ignore("test that") { fixture => theTestThatCalled = true }
      }

      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repC, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(repC.testIgnoredReceived)
      assert(repC.lastEvent.isDefined)
      assert(repC.lastEvent.get.testName endsWith "test that", repC.lastEvent.get.testName)
      assert(c.theTestThisCalled)
      assert(!c.theTestThatCalled)

      // The order I want is order of appearance in the file.
      // Will try and implement that tomorrow. Subtypes will be able to change the order.
      val d = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        ignore("test that") { fixture => theTestThatCalled = true }
      }

      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(repD.lastEvent.isDefined)
      assert(repD.lastEvent.get.testName endsWith "test that") // last because should be in order of appearance
      assert(!d.theTestThisCalled)
      assert(!d.theTestThatCalled)
    }

    it("should ignore a test marked as ignored if run is invoked with that testName") {
      // If I provide a specific testName to run, then it should ignore an Ignore on that test
      // method and actually invoke it.
      val e = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { fixture => theTestThisCalled = true }
        property("test that") { fixture => theTestThatCalled = true }
      }

      val repE = new TestIgnoredTrackingReporter
      e.run(Some("test this"), Args(repE, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(!e.theTestThatCalled)
    }

    it("should run only those tests selected by the tags to include and exclude sets") {

      // Nothing is excluded
      val a = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        property("test that") { fixture => theTestThatCalled = true }
      }
      val repA = new TestIgnoredTrackingReporter
      a.run(None, Args(repA, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(!repA.testIgnoredReceived)
      assert(a.theTestThisCalled)
      assert(a.theTestThatCalled)

      // SlowAsMolasses is included, one test should be excluded
      val b = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        property("test that") { fixture => theTestThatCalled = true }
      }
      val repB = new TestIgnoredTrackingReporter
      b.run(None, Args(repB, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), Map(), None, new Tracker, Set.empty))
      assert(!repB.testIgnoredReceived)
      assert(b.theTestThisCalled)
      assert(!b.theTestThatCalled)

      // SlowAsMolasses is included, and both tests should be included
      val c = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        property("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
      }
      val repC = new TestIgnoredTrackingReporter
      c.run(None, Args(repB, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set()), Map(), None, new Tracker, Set.empty))
      assert(!repC.testIgnoredReceived)
      assert(c.theTestThisCalled)
      assert(c.theTestThatCalled)

      // SlowAsMolasses is included. both tests should be included but one ignored
      val d = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this", mytags.SlowAsMolasses) { fixture => theTestThisCalled = true }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
      }
      val repD = new TestIgnoredTrackingReporter
      d.run(None, Args(repD, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore")), Map(), None, new Tracker, Set.empty))
      assert(repD.testIgnoredReceived)
      assert(!d.theTestThisCalled)
      assert(d.theTestThatCalled)

      // SlowAsMolasses included, FastAsLight excluded
      val e = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        property("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repE = new TestIgnoredTrackingReporter
      e.run(None, Args(repE, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker, Set.empty))
      assert(!repE.testIgnoredReceived)
      assert(!e.theTestThisCalled)
      assert(e.theTestThatCalled)
      assert(!e.theTestTheOtherCalled)

      // An Ignored test that was both included and excluded should not generate a TestIgnored event
      val f = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        property("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repF = new TestIgnoredTrackingReporter
      f.run(None, Args(repF, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker, Set.empty))
      assert(!repF.testIgnoredReceived)
      assert(!f.theTestThisCalled)
      assert(f.theTestThatCalled)
      assert(!f.theTestTheOtherCalled)

      // An Ignored test that was not included should not generate a TestIgnored event
      val g = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        ignore("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repG = new TestIgnoredTrackingReporter
      g.run(None, Args(repG, new Stopper {}, Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight")),
                Map(), None, new Tracker, Set.empty))
      assert(!repG.testIgnoredReceived)
      assert(!g.theTestThisCalled)
      assert(g.theTestThatCalled)
      assert(!g.theTestTheOtherCalled)

      // No tagsToInclude set, FastAsLight excluded
      val h = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        property("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repH = new TestIgnoredTrackingReporter
      h.run(None, Args(repH, new Stopper {}, Filter(None, Set("org.scalatest.FastAsLight")), Map(), None, new Tracker, Set.empty))
      assert(!repH.testIgnoredReceived)
      assert(!h.theTestThisCalled)
      assert(h.theTestThatCalled)
      assert(h.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded
      val i = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        property("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        property("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        property("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repI = new TestIgnoredTrackingReporter
      i.run(None, Args(repI, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses")), Map(), None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!i.theTestThisCalled)
      assert(!i.theTestThatCalled)
      assert(i.theTestTheOtherCalled)

      // No tagsToInclude set, SlowAsMolasses excluded, TestIgnored should not be received on excluded ones
      val j = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        var theTestTheOtherCalled = false
        ignore("test this", mytags.SlowAsMolasses, mytags.FastAsLight) { fixture => theTestThisCalled = true }
        ignore("test that", mytags.SlowAsMolasses) { fixture => theTestThatCalled = true }
        property("test the other") { fixture => theTestTheOtherCalled = true }
      }
      val repJ = new TestIgnoredTrackingReporter
      j.run(None, Args(repJ, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses")), Map(), None, new Tracker, Set.empty))
      assert(!repI.testIgnoredReceived)
      assert(!j.theTestThisCalled)
      assert(!j.theTestThatCalled)
      assert(j.theTestTheOtherCalled)

      // Same as previous, except Ignore specifically mentioned in excludes set
      val k = new FixturePropSpec {
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
      k.run(None, Args(repK, new Stopper {}, Filter(None, Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), Map(), None, new Tracker, Set.empty))
      assert(repK.testIgnoredReceived)
      assert(!k.theTestThisCalled)
      assert(!k.theTestThatCalled)
      assert(!k.theTestTheOtherCalled)
    }

    it("should return the correct test count from its expectedTestCount method") {

      val a = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        property("test this") { fixture => }
        property("test that") { fixture => }
      }
      assert(a.expectedTestCount(Filter()) === 2)

      val b = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        ignore("test this") { fixture => }
        property("test that") { fixture => }
      }
      assert(b.expectedTestCount(Filter()) === 1)

      val c = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        property("test this", mytags.FastAsLight) { fixture => }
        property("test that") { fixture => }
      }
      assert(c.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(c.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 1)

      val d = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        property("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => }
        property("test that", mytags.SlowAsMolasses) { fixture => }
        property("test the other thing") { fixture => }
      }
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.FastAsLight")), Set())) === 1)
      assert(d.expectedTestCount(Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.FastAsLight"))) === 1)
      assert(d.expectedTestCount(Filter(None, Set("org.scalatest.SlowAsMolasses"))) === 1)
      assert(d.expectedTestCount(Filter()) === 3)

      val e = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        property("test this", mytags.FastAsLight, mytags.SlowAsMolasses) { fixture => }
        property("test that", mytags.SlowAsMolasses) { fixture => }
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
      val a = new FixturePropSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }

        property("should do this") (pending)

        property("should do that") { fixture =>
          assert(fixture === hello)
        }
        
        property("should do something else") { fixture =>
          assert(fixture === hello)
          pending
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
    }

    it("should allow tests without fixtures to be combined with tests with fixtures") {

      val a = new FixturePropSpec {

        var theTestWithFixtureWasRun = false
        var theTestWithoutFixtureWasRun = false

        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }

        property("should do this") (pending)

        property("should do that") { fixture =>
          assert(fixture === hello)
          theTestWithFixtureWasRun = true
        }

        property("should do something else") { fixture =>
          assert(fixture === hello)
          pending
        }
        
        property("should do that without a fixture") { () =>
          assert(2 + 2 === 4)
          theTestWithoutFixtureWasRun = true
        }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      val tp = rep.testPendingEventsReceived
      assert(tp.size === 2)
      assert(a.theTestWithFixtureWasRun)
      assert(a.theTestWithoutFixtureWasRun)
    }
    it("should generate a test failure if a Throwable, or an Error other than direct Error subtypes " +
            "known in JDK 1.5, excluding AssertionError") {
      val a = new FixturePropSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }
        property("throws AssertionError") { s => throw new AssertionError }
        property("throws plain old Error") { s => throw new Error }
        property("throws Throwable") { s => throw new Throwable }
      }
      val rep = new EventRecordingReporter
      a.run(None, Args(rep, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      val tf = rep.testFailedEventsReceived
      assert(tf.size === 3)
    }
    it("should propagate out Errors that are direct subtypes of Error in JDK 1.5, other than " +
            "AssertionError, causing Suites and Runs to abort.") {
      val a = new FixturePropSpec {
        type FixtureParam = String
        val hello = "Hello, world!"
        def withFixture(test: OneArgTest) {
          test(hello)
        }
        property("throws AssertionError") { s => throw new OutOfMemoryError }
      }
      intercept[OutOfMemoryError] {
        a.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      }
    }
    it("should allow both tests that take fixtures and tests that don't") {
      val a = new FixturePropSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest) {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        property("take no args") { () => takesNoArgsInvoked = true }

        var takesAFixtureInvoked = false
        property("takes a fixture") { s => takesAFixtureInvoked = true }
      }

      a.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }

    it("should work with test functions whose inferred result type is not Unit") {
      val a = new FixturePropSpec {

        type FixtureParam = String
        def withFixture(test: OneArgTest) {
          test("Hello, world!")
        }

        var takesNoArgsInvoked = false
        property("take no args") { () => takesNoArgsInvoked = true; true }

        var takesAFixtureInvoked = false
        property("takes a fixture") { s => takesAFixtureInvoked = true; true }
      }

      assert(!a.takesNoArgsInvoked)
      assert(!a.takesAFixtureInvoked)
      a.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      assert(a.testNames.size === 2, a.testNames)
      assert(a.takesNoArgsInvoked)
      assert(a.takesAFixtureInvoked)
    }

    it("should work with ignored tests whose inferred result type is not Unit") {
      val a = new FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) { test("hi") }
        var theTestThisCalled = false
        var theTestThatCalled = false
        ignore("test this") { () => theTestThisCalled = true; "hi" }
        ignore("test that") { fixture => theTestThatCalled = true; 42 }
      }

      assert(!a.theTestThisCalled)
      assert(!a.theTestThatCalled)
      val reporter = new EventRecordingReporter
      a.run(None, Args(reporter, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      assert(reporter.testIgnoredEventsReceived.size === 2)
      assert(!a.theTestThisCalled)
      assert(!a.theTestThatCalled)
    }
    it("should pass a NoArgTest to withFixture for tests that take no fixture") {
      class MySuite extends FixturePropSpec {
        type FixtureParam = String
        var aNoArgTestWasPassed = false
        var aOneArgTestWasPassed = false
        override def withFixture(test: NoArgTest) {
          aNoArgTestWasPassed = true
        }
        def withFixture(test: OneArgTest) {
          aOneArgTestWasPassed = true
        }
        property("something") { () =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      assert(s.aNoArgTestWasPassed)
      assert(!s.aOneArgTestWasPassed)
    }
    it("should not pass a NoArgTest to withFixture for tests that take a Fixture") {
      class MySuite extends FixturePropSpec {
        type FixtureParam = String
        var aNoArgTestWasPassed = false
        var aOneArgTestWasPassed = false
        override def withFixture(test: NoArgTest) {
          aNoArgTestWasPassed = true
        }
        def withFixture(test: OneArgTest) {
          aOneArgTestWasPassed = true
        }
        property("something") { fixture =>
          assert(1 + 1 === 2)
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      assert(!s.aNoArgTestWasPassed)
      assert(s.aOneArgTestWasPassed)
    }
    it("should pass a NoArgTest that invokes the no-arg test when the " +
            "NoArgTest's no-arg apply method is invoked") {

      class MySuite extends FixturePropSpec {
        type FixtureParam = String
        var theNoArgTestWasInvoked = false
        def withFixture(test: OneArgTest) {
          // Shouldn't be called, but just in case don't invoke a OneArgTest
        }
        property("something") { () =>
          theNoArgTestWasInvoked = true
        }
      }

      val s = new MySuite
      s.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      assert(s.theNoArgTestWasInvoked)
    }

    it("should pass the correct test name in the OneArgTest passed to withFixture") {
      val a = new FixturePropSpec {
        type FixtureParam = String
        var correctTestNameWasPassed = false
        def withFixture(test: OneArgTest) {
          correctTestNameWasPassed = test.name == "something"
          test("hi")
        }
        property("something") { fixture => }
      }
      a.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker(), Set.empty))
      assert(a.correctTestNameWasPassed)
    }
    it("should pass the correct config map in the OneArgTest passed to withFixture") {
      val a = new FixturePropSpec {
        type FixtureParam = String
        var correctConfigMapWasPassed = false
        def withFixture(test: OneArgTest) {
          correctConfigMapWasPassed = (test.configMap == Map("hi" -> 7))
          test("hi")
        }
        property("something") { fixture => }
      }
      a.run(None, Args(SilentReporter, new Stopper {}, Filter(), Map("hi" -> 7), None, new Tracker(), Set.empty))
      assert(a.correctConfigMapWasPassed)
    }

    describe("(when a nesting rule has been violated)") {

      it("should, if they call a nested it from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          property("should blow up") { fixture =>
            property("should never run") { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested it with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          property("should blow up") { fixture =>
            property("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          property("should blow up") { fixture =>
            ignore("should never run") { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
      it("should, if they call a nested ignore with tags from within an it clause, result in a TestFailedException when running the test") {

        class MySuite extends FixturePropSpec {
          type FixtureParam = String
          def withFixture(test: OneArgTest) { test("hi") }
          property("should blow up") { fixture =>
            ignore("should never run", mytags.SlowAsMolasses) { fixture =>
              assert(1 === 1)
            }
          }
        }

        val spec = new MySuite
        ensureTestFailedEventReceived(spec, "should blow up")
      }
    }

    it("should throw IllegalArgumentException if passed a testName that doesn't exist") {
      class MySuite extends FixturePropSpec {
        type FixtureParam = String
        def withFixture(test: OneArgTest) {
          test("hi")
        }
        property("one") {s => () }
        property("two") {s => () }
      }
      val suite = new MySuite
      intercept[IllegalArgumentException] {
        suite.run(Some("three"), Args(SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker, Set.empty))
      }
    }
  }
}
