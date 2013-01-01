package org.scalatest.matchers

import org.scalatest._

class NounMatcherSpec extends Spec with ShouldMatchers {

  object `NounMatcher ` {
    
    def `can be used with integer should be` {
      val oddNumber = NounMatcher("odd number") { (x: Int) => x % 2 == 1 }
      1 should be (oddNumber)
    }
    
    def `can be used with integer should not be` {
      val oddNumber = NounMatcher("odd number") { (x: Int) => x % 2 == 1 }
      2 should not be (oddNumber)
    }
    
    def `can be used with string should be` {
      val oddNumberString = NounMatcher("odd number string") { (x: String) => x.length % 2 == 1 }
      "hello" should be (oddNumberString)
    }
    
    def `can be used with string should not be` {
      val oddNumberString = NounMatcher("odd number string") { (x: String) => x.length % 2 == 1 }
      "hi" should not be (oddNumberString)
    }
    
    def `can be used with list should be` {
      val oddNumberList = NounMatcher("odd number list") { (x: List[Int]) => x.length % 2 == 1 }
      List(1, 2, 3) should be (oddNumberList)
    }
    
    def `can be used with list should not be` {
      val oddNumberList = NounMatcher("odd number") { (x: List[Int]) => x.length % 2 == 1 }
      List(1, 2) should not be (oddNumberList)
    }
    
    def `can be used with set should be` {
      val oddNumberSet = NounMatcher("odd number list") { (x: Set[Int]) => x.size % 2 == 1 }
      Set(1, 2, 3) should be (oddNumberSet)
    }
    
    def `can be used with set should not be` {
      val oddNumberSet = NounMatcher("odd number") { (x: Set[Int]) => x.size % 2 == 1 }
      Set(1, 2) should not be (oddNumberSet)
    }
  }
  
}