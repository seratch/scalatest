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
package org.scalatest

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class AssertionsSpec extends FunSpec with OptionValues {

  describe("The === method") {
    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      assert(npe.getMessage === null)
    }
    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
    }
    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
      intercept[TestFailedException] {
        assert(a3 === a1)
      }
    }
    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      assert(n1 === n2)
      intercept[TestFailedException] {
        assert(n1 === "hi")
      }
      intercept[TestFailedException] {
        assert("hi" === n1)
      }
      val a1 = Array(1, 2, 3)
      intercept[TestFailedException] {
        assert(n1 === a1)
      }
      intercept[TestFailedException] {
        assert(a1 === n1)
      }
    }
  }
  describe("The intercept method") {
    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the TFE's cause") {
        val wrongException = new RuntimeException("oops!")
        val caught =
          intercept[TestFailedException] {
            intercept[IllegalArgumentException] {
              throw wrongException
            }
          }
        assert(caught.cause.value eq wrongException)
      }
    }
  }
  describe("The newAssert method") {
    it("should provides error message similar to using ===") {
      val a = 3
      val b = 5

      newAssert(a != b)

      val e1 = intercept[TestFailedException] { newAssert(a == 5) }
      assert(e1.getMessage === ("a did not equal to 5"))

      val e2 = intercept[TestFailedException] { newAssert(3 == b) }
      assert(e2.getMessage === ("3 did not equal to b"))

      val e3 = intercept[TestFailedException] {
        newAssert(3 == 5)
      }
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      assert(e3.getMessage === ("newAssert(3 == 5) fails."))

      val e4 = intercept[TestFailedException] { newAssert(a == b) }
      assert(e4.getMessage === ("a did not equal to b"))

      val e5 = intercept[TestFailedException] { newAssert(a == null) }
      assert(e5.getMessage === ("a did not equal to null"))

      val e6 = intercept[TestFailedException] { newAssert(null == a) }
      assert(e6.getMessage === ("null did not equal to a"))

      val e7 = intercept[TestFailedException] { newAssert(a != 3) }
      assert(e7.getMessage === ("a was equal to 3"))

      val e8 = intercept[TestFailedException] { newAssert(3 != a) }
      assert(e8.getMessage === ("3 was equal to a"))

      val e9 = intercept[TestFailedException] { newAssert(a > 3) }
      assert(e9.getMessage === ("a did not more than 3"))

      newAssert(a >= 3)

      val e10 = intercept[TestFailedException] { newAssert(a >= 4) }
      assert(e10.getMessage === ("a did not more than or equal 4"))

      val e11 = intercept[TestFailedException] { newAssert(b < 5) }
      assert(e11.getMessage === ("b did not less than 5"))

      newAssert(b <= 5)

      val e12 = intercept[TestFailedException] { newAssert(b <= 4) }
      assert(e12.getMessage === ("b did not less than or equal 4"))
    }
  }
}
