package org.scalatest.matchers

import org.scalatest._

class ContainMatcherSpec extends Spec with ShouldMatchers with SharedHelpers {

  object `theSameElementsAs ` {
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(1, 2, 3) should contain theSameElementsAs List(1, 2, 3)
    }
    
    def `should work with ContainMatcher directly` {
      val a = new TheSameElementsAsContainMatcher(List(1, 2, 3))
      List(1, 2, 3) should contain (a)
      Set(1, 2, 3) should contain (a)
    }
    
    def `should succeeded when left List contains same elements in different order as right List` {
      List(1, 2, 3) should contain theSameElementsAs List(2, 1, 3)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements` {
      val left = List(1, 2, 3)
      val right = List(2, 5, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      e.message should be (Some(left + " did not contain the same elements as " + right))
      e.failedCodeFileName should be (Some("ContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List` {
      val left = List(1, 2, 3)
      val right = List(1, 2, 3, 4)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      e.message should be (Some(left + " did not contain the same elements as " + right))
      e.failedCodeFileName should be (Some("ContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List` {
      val left = List(1, 2, 3)
      val right = List(1, 2)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      e.message should be (Some(left + " did not contain the same elements as " + right))
      e.failedCodeFileName should be (Some("ContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }
    
    def `should succeeded when left List contains same elements in different order as right Set` {
      List(1, 2, 3) should contain theSameElementsAs Set(2, 1, 3)
    }
    
    def `should succeeded when left List contains same elements in same order as right Set` {
      List(1, 2, 3) should contain theSameElementsAs Set(1, 2, 3)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List and right Set are same size but contain different elements` {
      val left = List(1, 2, 3)
      val right = Set(2, 5, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      e.message should be (Some(left + " did not contain the same elements as " + right))
      e.failedCodeFileName should be (Some("ContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right Set` {
      val left = List(1, 2, 3)
      val right = Set(1, 2, 3, 4)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      e.message should be (Some(left + " did not contain the same elements as " + right))
      e.failedCodeFileName should be (Some("ContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right Set` {
      val left = List(1, 2, 3)
      val right = Set(1, 2)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      e.message should be (Some(left + " did not contain the same elements as " + right))
      e.failedCodeFileName should be (Some("ContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }
    
  }
  
}