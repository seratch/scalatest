package org.scalatest.matchers

import org.scalatest._

class LoneElementSpec extends Spec with SharedHelpers {

  object `when used with ShouldMatchers` extends ShouldMatchers {
    
    def `should work with xs.loneElement and passed when xs only contains one element and the one element passed the check` {
      List(10).loneElement should be > 9
    }
    
    def `should throw TestFailedException with correct stack depth and message when xs.loneElement contains one element but it failed the check` {
      val e = intercept[exceptions.TestFailedException] {
        List(8).loneElement should be > 9
      }
      e.failedCodeFileName should be (Some("LoneElementSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
      e.message should be (Some("8 was not greater than 9"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when xs contains 0 element and xs.loneElement is called` {
      val xs = List.empty[Int]
      val e = intercept[exceptions.TestFailedException] {
        xs.loneElement should be > 9
      }
      e.failedCodeFileName should be (Some("LoneElementSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
      e.message should be (Some("Expected " + xs + " to contain exactly 1 element, but it has size 0"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when xs contains > 1 elements and xs.loneElement is called` {
      val xs = List(8, 12)
      val e = intercept[exceptions.TestFailedException] {
        xs.loneElement should be > 9
      }
      e.failedCodeFileName should be (Some("LoneElementSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
      e.message should be (Some("Expected " + xs + " to contain exactly 1 element, but it has size 2"))
    }
  }
  
  object `when used with LoneElement trait` extends LoneElement {
    def `should work with xs.loneElement and passed when xs only contains one element and the one element passed the check` {
      assert(List(10).loneElement > 9)
    }
    
    def `should throw TestFailedException with correct stack depth and message when xs.loneElement contains one element but it failed the check` {
      val e = intercept[exceptions.TestFailedException] {
        assert(List(8).loneElement > 9)
      }
      assert(e.failedCodeFileName === Some("LoneElementSpec.scala"))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 3))
      assert(e.message === None)
    }
    
    def `should throw TestFailedException with correct stack depth and message when xs contains 0 element and xs.loneElement is called` {
      val xs = List.empty[Int]
      val e = intercept[exceptions.TestFailedException] {
        assert(xs.loneElement > 9)
      }
      assert(e.failedCodeFileName == Some("LoneElementSpec.scala"))
      assert(e.failedCodeLineNumber == Some(thisLineNumber - 3))
      assert(e.message === Some("Expected List() to contain exactly 1 element, but it has size 0"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when xs contains > 1 elements and xs.loneElement is called` {
      val xs = List(8, 12)
      val e = intercept[exceptions.TestFailedException] {
        assert(xs.loneElement > 9)
      }
      assert(e.failedCodeFileName === Some("LoneElementSpec.scala"))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 3))
      assert(e.message === Some("Expected List(8, 12) to contain exactly 1 element, but it has size 2"))
    }
  }
  
  object `when used with both ShouldMatchers and LoneElement together` extends ShouldMatchers with LoneElement {
    
    def `should work with xs.loneElement and passed when should syntax is used and xs only contains one element and the one element passed the check` {
      List(10).loneElement should be > 9
    }
    
    def `should work with xs.loneElement and passed when assert syntax is used and xs only contains one element and the one element passed the check` {
      assert(List(10).loneElement > 9)
    }
    
    def `should throw TestFailedException with correct stack depth and message when should syntax is used and xs.loneElement contains one element but it failed the check` {
      val e = intercept[exceptions.TestFailedException] {
        List(8).loneElement should be > 9
      }
      e.failedCodeFileName should be (Some("LoneElementSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
      e.message should be (Some("8 was not greater than 9"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when assert syntax is used and xs.loneElement contains one element but it failed the check` {
      val e = intercept[exceptions.TestFailedException] {
        assert(List(8).loneElement > 9)
      }
      assert(e.failedCodeFileName === Some("LoneElementSpec.scala"))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 3))
      assert(e.message === None)
    }
    
    def `should throw TestFailedException with correct stack depth and message when should syntax is used and xs contains 0 element and xs.loneElement is called` {
      val xs = List.empty[Int]
      val e = intercept[exceptions.TestFailedException] {
        xs.loneElement should be > 9
      }
      e.failedCodeFileName should be (Some("LoneElementSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
      e.message should be (Some("Expected " + xs + " to contain exactly 1 element, but it has size 0"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when assert syntax is used and xs contains 0 element and xs.loneElement is called` {
      val xs = List.empty[Int]
      val e = intercept[exceptions.TestFailedException] {
        assert(xs.loneElement > 9)
      }
      assert(e.failedCodeFileName == Some("LoneElementSpec.scala"))
      assert(e.failedCodeLineNumber == Some(thisLineNumber - 3))
      assert(e.message === Some("Expected List() to contain exactly 1 element, but it has size 0"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when should syntax is used and xs contains > 1 elements and xs.loneElement is called` {
      val xs = List(8, 12)
      val e = intercept[exceptions.TestFailedException] {
        xs.loneElement should be > 9
      }
      e.failedCodeFileName should be (Some("LoneElementSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
      e.message should be (Some("Expected " + xs + " to contain exactly 1 element, but it has size 2"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when assert syntax is used and xs contains > 1 elements and xs.loneElement is called` {
      val xs = List(8, 12)
      val e = intercept[exceptions.TestFailedException] {
        assert(xs.loneElement > 9)
      }
      assert(e.failedCodeFileName === Some("LoneElementSpec.scala"))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 3))
      assert(e.message === Some("Expected List(8, 12) to contain exactly 1 element, but it has size 2"))
    }
  }
}
