package org.scalatest.matchers

import org.scalatest._

class NounMatcherSpec extends Spec with ShouldMatchers with SharedHelpers {

  object `NounMatcher ` {
    
    object `when used with integer` {
      val oddNumber = NounMatcher("odd number") { (x: Int) => x % 2 == 1 }
      
      def `should work correctly with 'should be'` {
        1 should be (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          2 should be (oddNumber)
        }
        e.message should be (Some("2 was not an odd number"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        2 should not be (oddNumber)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          1 should not be (oddNumber)
        }
        e.message should be (Some("1 was an odd number"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with string` {
      val oddNumberString = NounMatcher("odd number string") { (x: String) => x.length % 2 == 1 }
      
      def `should work correctly with 'should be'` {
        "hello" should be (oddNumberString)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          "helloo" should be (oddNumberString)
        }
        e.message should be (Some("\"helloo\" was not an odd number string"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        "hi" should not be (oddNumberString)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          "hai" should not be (oddNumberString)
        }
        e.message should be (Some("\"hai\" was an odd number string"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with list` {
      val oddNumberList = NounMatcher("odd number list") { (x: List[Int]) => x.length % 2 == 1 }
      
      def `should work correctly with 'should be'` {
        List(1, 2, 3) should be (oddNumberList)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2) should be (oddNumberList)
        }
        e.message should be (Some(List(1, 2) + " was not an odd number list"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        List(1, 2) should not be (oddNumberList)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2, 3) should not be (oddNumberList)
        }
        e.message should be (Some(List(1, 2, 3) + " was an odd number list"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with set` {
      val oddNumberSet = NounMatcher("odd number set") { (x: Set[Int]) => x.size % 2 == 1 }
      
      def `should work correctly with 'should be'` {
        Set(1, 2, 3) should be (oddNumberSet)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set(1, 2) should be (oddNumberSet)
        }
        e.message should be (Some(Set(1, 2) + " was not an odd number set"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Set(1, 2) should not be (oddNumberSet)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Set(1, 2, 3) should not be (oddNumberSet)
        }
        e.message should be (Some(Set(1, 2, 3) + " was an odd number set"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with array` {
      val oddNumberArray = NounMatcher("odd number array") { (x: Array[Int]) => x.size % 2 == 1 }
      
      def `should work correctly with 'should be'` {
        Array(1, 2, 3) should be (oddNumberArray)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array(1, 2) should be (oddNumberArray)
        }
        e.message should be (Some(Array(1, 2).deep + " was not an odd number array"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Array(1, 2) should not be (oddNumberArray)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Array(1, 2, 3) should not be (oddNumberArray)
        }
        e.message should be (Some(Array(1, 2, 3).deep + " was an odd number array"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with map` {
      val oddNumberMap = NounMatcher("odd number map") { (x: Map[Int, String]) => x.size % 2 == 1 }
      
      def `should work correctly with 'should be'` {
        Map(1 -> "one", 2 -> "two", 3 -> "three") should be (oddNumberMap)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map(1 -> "one", 2 -> "two") should be (oddNumberMap)
        }
        e.message should be (Some(Map(1 -> "one", 2 -> "two") + " was not an odd number map"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        Map(1 -> "one", 2 -> "two") should not be (oddNumberMap)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be NounMatcher' assertion failed` {
        val e = intercept[exceptions.TestFailedException] {
          Map(1 -> "one", 2 -> "two", 3 -> "three") should not be (oddNumberMap)
        }
        e.message should be (Some(Map(1 -> "one", 2 -> "two", 3 -> "three") + " was an odd number map"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with java collection` {
      val oddNumberJavaCol = NounMatcher("odd number java collection") { (x: java.util.List[Int]) => x.size % 2 == 1 }
      
      def `should work correctly with 'should be'` {
        val javaList = new java.util.ArrayList[Int]()
        javaList.add(1)
        javaList.add(2)
        javaList.add(3)
        javaList should be (oddNumberJavaCol)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be NounMatcher' assertion failed` {
        val javaList = new java.util.ArrayList[Int]()
        javaList.add(1)
        javaList.add(2)
        val e = intercept[exceptions.TestFailedException] {
          javaList should be (oddNumberJavaCol)
        }
        e.message should be (Some(javaList + " was not an odd number java collection"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        val javaList = new java.util.ArrayList[Int]()
        javaList.add(1)
        javaList.add(2)
        javaList should not be (oddNumberJavaCol)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be NounMatcher' assertion failed` {
        val javaList = new java.util.ArrayList[Int]()
        javaList.add(1)
        javaList.add(2)
        javaList.add(3)
        val e = intercept[exceptions.TestFailedException] {
          javaList should not be (oddNumberJavaCol)
        }
        e.message should be (Some(javaList + " was an odd number java collection"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    object `when used with java map` {
      val oddNumberJavaMap = NounMatcher("odd number java map") { (x: java.util.Map[Int, String]) => x.size % 2 == 1 }
      
      def `should work correctly with 'should be'` {
        val javaMap = new java.util.HashMap[Int, String]()
        javaMap.put(1, "one")
        javaMap.put(2, "two")
        javaMap.put(3, "three")
        javaMap should be (oddNumberJavaMap)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should be NounMatcher' assertion failed` {
        val javaMap = new java.util.HashMap[Int, String]()
        javaMap.put(1, "one")
        javaMap.put(2, "two")
        val e = intercept[exceptions.TestFailedException] {
          javaMap should be (oddNumberJavaMap)
        }
        e.message should be (Some(javaMap + " was not an odd number java map"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      def `should work correctly with 'should not be'` {
        val javaMap = new java.util.HashMap[Int, String]()
        javaMap.put(1, "one")
        javaMap.put(2, "two")
        javaMap should not be (oddNumberJavaMap)
      }
      
      def `should throw TestFailedException with correct stack depth and message when 'should not be NounMatcher' assertion failed` {
        val javaMap = new java.util.HashMap[Int, String]()
        javaMap.put(1, "one")
        javaMap.put(2, "two")
        javaMap.put(3, "three")
        val e = intercept[exceptions.TestFailedException] {
          javaMap should not be (oddNumberJavaMap)
        }
        e.message should be (Some(javaMap + " was an odd number java map"))
        e.failedCodeFileName should be (Some("NounMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
  }
  
}