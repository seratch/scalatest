package org.scalatest.matchers

import Matchers._

class MatcherSpec extends Spec {
  "The equal matcher" -- {
    "should do nothing when equal" - {
      1 should equal (1)
    }
    "should throw an assertion error when not equal" - {
      intercept(classOf[AssertionError]) {
        1 should equal (2)
      }
    }
  }
  "The be matcher" -- {

    "should do nothing when false is compared to false" - {
      false should be (false)
    }

    "should do nothing when true is compared to true" - {
      true should be (true)
    }

    "should throw an assertion error when not equal" - {
      intercept(classOf[AssertionError]) {
        false should be (true)
      }
    }

    "should do nothing when null is compared to null" - {
      val o: String = null
      o should be (null)
    }

    "should throw an assertion error when non-null compared to null" - {
      intercept(classOf[AssertionError]) {
        val o = "Helloooooo"
        o should be (null)
      }
    }

    "should do nothing when non-null is compared to not null" - {
      val o = "Helloooooo"
      o should not { be (null) }
    }

    "should throw an assertion error when null compared to not null" - {
      intercept(classOf[AssertionError]) {
        val o: String = null
        o should not { be (null) }
      }
    }

    "should call isEmpty when passed 'empty" - {
      val emptySet = Set()
      emptySet should be ('empty)
      val nonEmptySet = Set(1, 2, 3)
      nonEmptySet should not { be ('empty) }
    }

    "should be invokable from beA(Symbol) and beAn(Symbol)" - {
      val emptySet = Set()
      emptySet should beA ('empty)
      emptySet should beAn ('empty)
      val nonEmptySet = Set(1, 2, 3)
      nonEmptySet should not { beA ('empty) }
      nonEmptySet should not { beAn ('empty) }
    }

    "should call empty when passed 'empty" - {
      class EmptyMock {
        def empty: Boolean = true
      }
      class NonEmptyMock {
        def empty: Boolean = false
      }
      (new EmptyMock) should be ('empty)
      (new NonEmptyMock) should not { be ('empty) }
    }

    "should throw IllegalArgumentException if no empty or isEmpty method" - {
      class EmptyMock {
        override def toString = "EmptyMock"
      }
      class NonEmptyMock {
        override def toString = "NonEmptyMock"
      }
      val ex1 = intercept(classOf[IllegalArgumentException]) {
        (new EmptyMock) should be ('empty)
      }
      ex1.getMessage should equal ("EmptyMock has neither a empty or a isEmpty method.")
      val ex2 = intercept(classOf[IllegalArgumentException]) {
        (new NonEmptyMock) should not { be ('empty) }
      }
      ex2.getMessage should equal ("NonEmptyMock has neither a empty or a isEmpty method.")
    }

    "should throw IllegalArgumentException if both an empty and an isEmpty method exist" - {
      class EmptyMock {
        def empty: Boolean = true
        def isEmpty: Boolean = true
        override def toString = "EmptyMock"
      }
      class NonEmptyMock {
        def empty: Boolean = true
        def isEmpty: Boolean = true
        override def toString = "NonEmptyMock"
      }
      val ex1 = intercept(classOf[IllegalArgumentException]) {
        (new EmptyMock) should be ('empty)
      }
      ex1.getMessage should equal ("EmptyMock has both a empty and a isEmpty method.")
      val ex2 = intercept(classOf[IllegalArgumentException]) {
        (new NonEmptyMock) should not { be ('empty) }
      }
      ex2.getMessage should equal ("NonEmptyMock has both a empty and a isEmpty method.")
    }
    "should access an 'empty' val when passed 'empty" - {
      class EmptyMock {
        val empty: Boolean = true
      }
      class NonEmptyMock {
        val empty: Boolean = false
      }
      (new EmptyMock) should be ('empty)
      (new NonEmptyMock) should not { be ('empty) }
    }
  }
  "The not matcher" -- {
    "should do nothing when not true" - {
      1 should not { equal (2) }
    }
    "should throw an assertion error when true" - {
      intercept(classOf[AssertionError]) {
        1 should not { equal (1) }
      }
    }
  }
  "The endsWith matcher" -- {
    "should do nothing when true" - {
      "Hello, world" should endWith ("world")
    }
    "should throw an assertion error when not true" - {
      intercept(classOf[AssertionError]) {
        "Hello, world" should endWith ("planet")
      }
    }
  }

  "The and matcher" -- {
    "should do nothing when both operands are true" - {
      1 should { equal (1) and equal (2 - 1) }
    }
    "should throw AssertionError when first operands is false" - {
      intercept(classOf[AssertionError]) {
        1 should (equal (2) and equal (1))
      }
    }
    "should throw AssertionError when second operands is false" - {
      intercept(classOf[AssertionError]) {
        1 should (equal (1) and equal (2))
      }
    }
    "should not execute the right matcher creation function when the left operand is false" - {
      var called = false
      def mockMatcher = new Matcher[Int] { def apply(i: Int) = { called = true; MatcherResult(true, "", "") } }
      intercept(classOf[AssertionError]) {
        // This should fail, but without applying the matcher returned by mockMatcher
        1 should { equal (2) and mockMatcher }
      }
      called should be (false)
    }
    "should execute the right matcher creation function when the left operand is true" - {
      var called = false
      def mockMatcher = new Matcher[Int] { def apply(i: Int) = { called = true; MatcherResult(true, "", "") } }
      1 should { equal (1) and mockMatcher }
      called should be (true)
      // mySet should not { be (empty) }
    }
    /*
     map should haveKey (8)
     map should haveValue ("eleven")
     iterable should contain (42)
     iterable should haveSize (3)
     string should haveLength (8)
     array should haveLength (9)
     object should be ('empty)
     object should beA ('file)
     object should beAn ('openBook)
     list should be (Nil)
     object should be { anInstanceOf[Something] }
     object should be { theSameInstanceAs(anotherObjectReference) }
     string should equalIgnoringCase ("happy")
     string should equalTrimmed (" happy  ")
     string should startWith ("something")
     string should include ("substring for which indexOf > -1")
     string should matchRegEx ("""[a-zA-Z_]\w*""")

     // This could work. greaterThan has an apply method, and it has
     // an "or" method that takes whatever equalTo(7) returns
     // Maybe these could be structural.
     number should be greaterThan (7)
     number should be greaterThan or equalTo (7)
     number should be lessThan (7)
     number should be lessThan or equalTo (7)

     // number should beGreaterThan (7)
     // number should beGreaterThanOrEqualTo (7)
     // number should beLessThan (7)
     // number should beLessThanOrEqualTo (7)

     floatingPointNumber should be { 7.0 plusOrMinus 0.01 }
     floatingPointNumber should be { 7.0 exactly }
     // floatingPointNumber should beCloseTo { 7.0 withinTolerance 0.01 }
     option should be (None)
     option should equal (Some(1))
     option should not { be (None) } // for any Some
     option should be (aSome[String]) // or this
     option should be (aSome[String], which should startWith ("prefix")) // or this
     object should satisfy (_ > 12)
     object should matchPattern { case 1 :: _ :: 3 :: Nil => true } // or..
     object should satisfy { case 1 :: _ :: 3 :: Nil => true } 

     { "Howdy".charAt(-1) } shouldThrow (classOf[StringIndexOutOfBoundsException])

     theFollowing shouldThrow (classOf[StringIndexOutOfBoundsException]) {
       "Howdy".charAt(-1)
     }
    */
  }
}
