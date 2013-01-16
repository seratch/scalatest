package org.scalatest

class ContainMatcherAndOrSpec extends Spec with Matchers with SharedHelpers {

  object `ContainMatcher ` {
    
    object `when use with 'and'` {
      
      def `should pass when both contain passes` {
        val left = List(1, 2, 3)
        val right1 = List(3, 1, 2)
        val right2 = List(1, 2, 3)
        left should (contain theSameElementsAs (right1) and contain theSameElementsAs (right2)) 
        left should (contain theSameElementsAs (right1) and contain theSameIteratedElementsAs (right2)) 
        left should (contain theSameElementsAs (right1) and contain allOf (3, 2, 1))
        left should (contain theSameElementsAs (right1) and contain inOrder (1, 2, 3))
        left should (contain theSameElementsAs (right1) and contain oneOf (1, 3, 5))
        left should (contain theSameElementsAs (right1) and contain only (3, 1, 2))
        left should (contain theSameElementsAs (right1) and contain inOrderOnly (1, 2, 3))
        left should (contain theSameElementsAs (right1) and contain noneOf (7, 8, 9))
        
        left should (contain theSameIteratedElementsAs (right2) and contain theSameElementsAs (right1))
        left should (contain allOf (3, 2, 1) and contain theSameElementsAs (right1))
        left should (contain inOrder (1, 2, 3) and contain theSameElementsAs (right1))
        left should (contain oneOf (1, 3, 5) and contain theSameElementsAs (right1))
        left should (contain only (3, 1, 2) and contain theSameElementsAs (right1))
        left should (contain inOrderOnly (1, 2, 3) and contain theSameElementsAs (right1))
        left should (contain noneOf (7, 8, 9) and contain theSameElementsAs (right1))
      }
      
      def `should failed with correctly stack depth and message when one of contain failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2, 3) should (contain theSameElementsAs List(3, 2, 1) and contain theSameIteratedElementsAs List(3, 2, 1)) 
        }
        e.message should be (Some("List(1, 2, 3) contained the same elements as List(3, 2, 1), but List(1, 2, 3) did not contain the same iterated elements as List(3, 2, 1)"))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when use with 'or'` {
      
      def `should pass when one of contain passes` {
        val left = List(1, 2, 3)
        val right1 = List(5, 1, 2)
        val right2 = List(1, 2, 3)
        left should (contain theSameElementsAs (right1) or contain theSameElementsAs (right2)) 
        left should (contain theSameElementsAs (right1) or contain theSameIteratedElementsAs (right2)) 
        left should (contain theSameElementsAs (right1) or contain allOf (3, 2, 1))
        left should (contain theSameElementsAs (right1) or contain inOrder (1, 2, 3))
        left should (contain theSameElementsAs (right1) or contain oneOf (1, 3, 5))
        left should (contain theSameElementsAs (right1) or contain only (3, 1, 2))
        left should (contain theSameElementsAs (right1) or contain inOrderOnly (1, 2, 3))
        left should (contain theSameElementsAs (right1) or contain noneOf (7, 8, 9))
        
        left should (contain theSameIteratedElementsAs (right2) or contain theSameElementsAs (right1))
        left should (contain allOf (3, 2, 1) or contain theSameElementsAs (right1))
        left should (contain inOrder (1, 2, 3) or contain theSameElementsAs (right1))
        left should (contain oneOf (1, 3, 5) or contain theSameElementsAs (right1))
        left should (contain only (3, 1, 2) or contain theSameElementsAs (right1))
        left should (contain inOrderOnly (1, 2, 3) or contain theSameElementsAs (right1))
        left should (contain noneOf (7, 8, 9) or contain theSameElementsAs (right1))
      }
      
      def `should failed with correctly stack depth and message when both of contain failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2, 3) should (contain theSameElementsAs List(3, 8, 1) or contain theSameIteratedElementsAs List(3, 2, 1)) 
        }
        e.message should be (Some("List(1, 2, 3) did not contain the same elements as List(3, 8, 1), and List(1, 2, 3) did not contain the same iterated elements as List(3, 2, 1)"))
        e.failedCodeFileName should be (Some("ContainMatcherAndOrSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
  }
  
}