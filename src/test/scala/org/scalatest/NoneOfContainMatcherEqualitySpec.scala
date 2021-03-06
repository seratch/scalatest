package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly
import collection.GenTraversable

class NoneOfContainMatcherEqualitySpec extends Spec with Matchers with Explicitly with SharedHelpers {

  class TrimEquality extends Equality[String] {
    def areEqual(left: String, right: Any) = 
      left.trim == (right match {
        case s: String => s.trim
        case other => other
      })
  }
  
  class MapTrimEquality extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any) = 
      right match {
        case t2: Tuple2[_, _] =>  
          left._1 == t2._1 && 
          left._2.trim == (t2._2 match {
            case s: String => s.trim
            case other => other
          })
        case right => left == right
      }
  }
  
  class FalseEquality extends Equality[Int] {
    def areEqual(left: Int, right: Any): Boolean = false
  }
  
  class MapFalseEquality extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any): Boolean = false
  }
  
  class SetEquality(validLeft: Set[Int], validRight: Set[Any], returnValue: Boolean) extends Equality[Int] {
    def areEqual(left: Int, right: Any): Boolean = 
      if (validLeft.contains(left) && validRight.contains(right))
        returnValue
      else
        !returnValue
  }
  
  class MapSetEquality(validLeft: Set[(Int, String)], validRight: Set[Any], returnValue: Boolean) extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any): Boolean = 
      if (validLeft.contains(left) && validRight.contains(right))
        returnValue
      else
        !returnValue
  }
  
  object `noneOf ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("NoneOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("NoneOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take custom implicit equality in scope when 'should contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should contain noneOf (1, 2, 3)
      Set(1, 2, 3) should contain noneOf (1, 2, 3)
      Array(1, 2, 3) should contain noneOf (1, 2, 3)
      javaList(1, 2, 3) should contain noneOf (1, 2, 3)
      javaSet(1, 2, 3) should contain noneOf (1, 2, 3)
        
      implicit val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> " two", 3 -> "three"), Set(1 -> "one", 2 -> " two", 3 -> "three"), false)
      Map(1 -> "one", 2 -> " two", 3 -> "three") should contain noneOf (1 -> "one", 2 -> " two", 3 -> "three")
      javaMap(1 -> "one", 2 -> " two", 3 -> "three") should contain noneOf (1 -> "one", 2 -> " two", 3 -> "three")
    }
    
    def `should take custom implicit equality in scope when 'should not contain' is used` {
      implicit val equality = new SetEquality(Set(1, 2, 3), Set(7, 8, 9), true)
      List(1, 2, 3) should not contain noneOf (7, 8, 9)
      Set(1, 2, 3) should not contain noneOf (7, 8, 9)
      Array(1, 2, 3) should not contain noneOf (7, 8, 9)
      javaList(1, 2, 3) should not contain noneOf (7, 8, 9)
      javaSet(1, 2, 3) should not contain noneOf (7, 8, 9)
      
      implicit val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(7 -> "seven", 8 -> "eight", 9 -> "nine"), true)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain noneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain noneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new SetEquality(Set(1, 2, 3), Set(6, 7, 8), true)
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain noneOf (6, 7, 8)
      }
      checkShouldContainStackDepth(e1, left1, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain noneOf (6, 7, 8)
      }
      checkShouldContainStackDepth(e2, left2, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain noneOf (6, 7, 8)
      }
        checkShouldContainStackDepth(e3, left3, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain noneOf (6, 7, 8)
      }
      checkShouldContainStackDepth(e4, left4, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      implicit val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(6 -> "six", 7 -> "seven", 8 -> "eight"), true)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
      }
      checkShouldContainStackDepth(e5, left5, Array(6 -> "six", 7 -> "seven", 8 -> "eight").deep, thisLineNumber - 2)
      
      val left6 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
      }
      checkShouldContainStackDepth(e6, left6, Array(6 -> "six", 7 -> "seven", 8 -> "eight").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new SetEquality(Set(1, 2, 3), Set(1, 2, 3), false)
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain noneOf (1, 2, 3)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain noneOf (1, 2, 3)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain noneOf (1, 2, 3)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain noneOf (1, 2, 3)
      }
      checkShouldNotContainStackDepth(e4, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      implicit val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(1 -> "one", 2 -> "two", 3 -> "three"), false)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain noneOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkShouldNotContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
      
      val left6 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain noneOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkShouldNotContainStackDepth(e6, left6, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
    }
    
    def `should take custom explicit equality in scope when 'should contain' is used` {
      val equality = new FalseEquality
      (List(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
      (Set(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
      (Array(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
      (javaList(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
      (javaSet(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
        
      val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> " two", 3 -> "three"), Set(1 -> "one", 2 -> " two", 3 -> "three"), false)
      (Map(1 -> "one", 2 -> " two", 3 -> "three") should contain noneOf (1 -> "one", 2 -> " two", 3 -> "three")) (mapEquality)
      (javaMap(1 -> "one", 2 -> " two", 3 -> "three") should contain noneOf (1 -> "one", 2 -> " two", 3 -> "three")) (mapEquality)
    }
    
    def `should take custom explicit equality in scope when 'should not contain' is used` {
      val equality = new SetEquality(Set(1, 2, 3), Set(7, 8, 9), true)
      List(1, 2, 3) should not contain noneOf (7, 8, 9) (equality)
      Set(1, 2, 3) should not contain noneOf (7, 8, 9) (equality)
      Array(1, 2, 3) should not contain noneOf (7, 8, 9) (equality)
      javaList(1, 2, 3) should not contain noneOf (7, 8, 9) (equality)
      javaSet(1, 2, 3) should not contain noneOf (7, 8, 9) (equality)
      
      val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(7 -> "seven", 8 -> "eight", 9 -> "nine"), true)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain noneOf (7 -> "seven", 8 -> "eight", 9 -> "nine") (mapEquality)
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain noneOf (7 -> "seven", 8 -> "eight", 9 -> "nine") (mapEquality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality in scope` {
      val equality = new SetEquality(Set(1, 2, 3), Set(6, 7, 8), true)
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain noneOf (6, 7, 8)) (equality)
      }
      checkShouldContainStackDepth(e1, left1, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain noneOf (6, 7, 8)) (equality)
      }
      checkShouldContainStackDepth(e2, left2, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain noneOf (6, 7, 8)) (equality)
      }
        checkShouldContainStackDepth(e3, left3, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain noneOf (6, 7, 8)) (equality)
      }
      checkShouldContainStackDepth(e4, left4, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(6 -> "six", 7 -> "seven", 8 -> "eight"), true)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")) (mapEquality)
      }
      checkShouldContainStackDepth(e5, left5, Array(6 -> "six", 7 -> "seven", 8 -> "eight").deep, thisLineNumber - 2)
      
      val left6 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")) (mapEquality)
      }
      checkShouldContainStackDepth(e6, left6, Array(6 -> "six", 7 -> "seven", 8 -> "eight").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality in scope` {
      val equality = new SetEquality(Set(1, 2, 3), Set(1, 2, 3), false)
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain noneOf (1, 2, 3) (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain noneOf (1, 2, 3) (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain noneOf (1, 2, 3) (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain noneOf (1, 2, 3) (equality)
      }
      checkShouldNotContainStackDepth(e4, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(1 -> "one", 2 -> "two", 3 -> "three"), false)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain noneOf (1 -> "one", 2 -> "two", 3 -> "three") (mapEquality)
      }
      checkShouldNotContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
      
      val left6 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain noneOf (1 -> "one", 2 -> "two", 3 -> "three") (mapEquality)
      }
      checkShouldNotContainStackDepth(e6, left6, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
    }
    
  }
  
}