package org.scalatest.akka

import org.scalatest.FunSpec
import AkkaEventually._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.exceptions.TestFailedException
import org.scalatest.time.Span
import org.scalatest.time.Millis

class AkkaEventuallySpec extends FunSpec with ShouldMatchers {

  describe("Akka Eventually ") {
    it("should read and use span scale factor from Akka configuration, and scale span correctly.") {
      assert(spanScaleFactor == 2.0)
      var startTime: Option[Long] = None
      evaluating {
        startTime = Some(System.currentTimeMillis)
        eventually(timeout(scaled(Span(500, Millis)))) {
          1 + 1 should equal (3)
        }
      } should produce [TestFailedException]
      (System.currentTimeMillis - startTime.get).toInt should be >= (1000)
    }
    
    it("should not scale span that is not called with scaled") {
      assert(spanScaleFactor == 2.0)
      var startTime: Option[Long] = None
      evaluating {
        startTime = Some(System.currentTimeMillis)
        eventually(timeout(Span(500, Millis))) {
          1 + 1 should equal (3)
        }
      } should produce [TestFailedException]
      val duration = (System.currentTimeMillis - startTime.get).toInt
      duration should be >= (500)
      duration should be < (1000)
    }
  }
  
}