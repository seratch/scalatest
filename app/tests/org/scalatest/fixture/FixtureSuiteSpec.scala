/*
 * Copyright 2001-2008 Artima, Inc.
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

import collection.immutable.TreeSet

class FixtureSuiteSpec extends Spec with PrivateMethodTester {

  describe("The private testMethodTakesInformer method") {
    val testMethodTakesInformer = PrivateMethod[Boolean]('testMethodTakesInformer)
    val suiteObject = org.scalatest.fixture.BaseFixtureSuite
    it("should return true if passed a string that ends in (Fixture, Informer)") {
      assert(suiteObject invokePrivate testMethodTakesInformer("thisDoes(Fixture, Informer)"))
      assert(suiteObject invokePrivate testMethodTakesInformer("(Fixture, Informer)"))
      assert(suiteObject invokePrivate testMethodTakesInformer("test(Fixture, Informer)"))
    }
    it("should return false if passed a string that doesn't end in (Fixture, Informer)") {
      assert(!(suiteObject invokePrivate testMethodTakesInformer("thisDoesNot(Fixture)")))
      assert(!(suiteObject invokePrivate testMethodTakesInformer("test(Fixture)")))
    }
  }

  describe("A FixtureSuite") {
    it("should return the test names in alphabetical order from testNames") {
      val a = new BaseFixtureSuite with BasicFixture {
        type Fixture = String
        def withFixture(fun: String => Unit) {}
        def testThis(fixture: String) {}
        def testThat(fixture: String) {}
      }

      expect(List("testThat(Fixture)", "testThis(Fixture)")) {
        a.testNames.elements.toList
      }

      val b = new BaseFixtureSuite with BasicFixture {
        type Fixture = String
        def withFixture(fun: String => Unit) {}
      }

      expect(List[String]()) {
        b.testNames.elements.toList
      }

      val c = new BaseFixtureSuite with BasicFixture {
        type Fixture = String
        def withFixture(fun: String => Unit) {}
        def testThat(fixture: String) {}
        def testThis(fixture: String) {}
      }

      expect(List("testThat(Fixture)", "testThis(Fixture)")) {
        c.testNames.elements.toList
      }
    }

    it("should discover tests with and without Informer parameters") {
      val a = new BaseFixtureSuite with BasicFixture {
        type Fixture = String
        def withFixture(fun: String => Unit) {}
        def testThis(fixture: String) = ()
        def testThat(fixture: String, info: Informer) = ()
      }
      assert(a.testNames === TreeSet("testThat(Fixture, Informer)", "testThis(Fixture)"))
    }

    it("should pass in the fixture to every test method") {
      val a = new BaseFixtureSuite with BasicFixture {
        type Fixture = String
        val hello = "Hello, world!"
        def withFixture(fun: String => Unit) {
          fun(hello)
        }
        def testThis(fixture: String) {
          assert(fixture === hello)
        }
        def testThat(fixture: String, info: Informer) {
          assert(fixture === hello)
        }
      }
      assert(a.testNames === TreeSet("testThat(Fixture, Informer)", "testThis(Fixture)"))
    }
  }
}
