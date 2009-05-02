package org.scalatest

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._

class OrdinalSpec extends Spec with ShouldMatchers with Checkers {

  describe("An Ordinal") {

    it("should produce a runStamp :: N on nth next") {
      check(
        (count: Byte) => {
          (count >= 0) ==> {
            var ord = new Ordinal(99)
            for (i <- 0 until count)
              ord = ord.next
            ord.toList == List(99, count)
          }
        }
      )
    }

    it("should produce a runStamp :: 0 ... n times on nth nextForNewSuite") {
      check(
        (count: Byte) => {
          (count >= 0) ==> {
            var ord = new Ordinal(99)
            for (i <- 0 until count)
              ord = ord.nextForNewSuite._1
            ord.toList == 99 :: List.make(count + 1, 0)
          }
        }
      )
    }

    it("should produce a runStamp :: 0 :: 1 :: 2 :: ... :: n on nth next ad nextForNewSuite") {
      check(
        (count: Byte) => {
          (count >= 0) ==> {
            var ord = new Ordinal(99)
            for (i <- 0 until count) {
              for (j <- 0 until i) {
                ord = ord.next
                // println("INNER: " + ord.toList)
              }
              ord = ord.nextForNewSuite._1
              // println("OUTER: " + ord.toList)
            }
            for (i <- 0 until count) // Get the nth one up to be count
              ord = ord.next
            // println("COUNT: " + count + " FINAL: " + ord.toList)
            val zeroToCount = for (i <- 0 to count) yield i
            // println("ZERO2COUNT: " + zeroToCount)
            ord.toList == 99 :: zeroToCount.toList
          }
        }
      )
    }
  }
}
