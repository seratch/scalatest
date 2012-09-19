package org.scalatest.matchers

import org.scalatest._
import Helper._
import scala.util.matching.Regex
import Assertions.areEqualComparingArraysStructurally

trait MatchersBase { matchers =>
  
  // TODO: Can probably rewrite this with a Thread.currentStackTrace or whatever the method is. No need
  // to create the temporary RuntimeException
  private[scalatest] def newTestFailedException(message: String, optionalCause: Option[Throwable] = None): Throwable = {
    val fileNames = List("Matchers.scala", "ShouldMatchers.scala", "MustMatchers.scala")
    val temp = new RuntimeException
    val stackDepth = temp.getStackTrace.takeWhile(stackTraceElement => fileNames.exists(_ == stackTraceElement.getFileName) || stackTraceElement.getMethodName == "newTestFailedException").length
    // if (stackDepth != 4) throw new OutOfMemoryError("stackDepth in Matchers.scala is: " + stackDepth)
    optionalCause match {
      case Some(cause) => new TestFailedException(message, cause, stackDepth)
      case None => new TestFailedException(message, stackDepth)
    }
  }
  
  private[scalatest] def matchSymbolToPredicateMethod[S <: AnyRef](left: S, right: Symbol, hasArticle: Boolean, articleIsA: Boolean): MatchResult = {

    // If 'empty passed, rightNoTick would be "empty"
    val propertyName = right.name

    accessProperty(left, right, true) match {

      case None =>

        // if propertyName is '>, mangledPropertyName would be "$greater"
        val mangledPropertyName = transformOperatorChars(propertyName)

        // methodNameToInvoke would also be "empty"
        val methodNameToInvoke = mangledPropertyName

        // methodNameToInvokeWithIs would be "isEmpty"
        val methodNameToInvokeWithIs = "is"+ mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

        val firstChar = propertyName(0).toLower
        val methodNameStartsWithVowel = firstChar == 'a' || firstChar == 'e' || firstChar == 'i' ||
          firstChar == 'o' || firstChar == 'u'

        throw newTestFailedException(
          FailureMessages(
            if (methodNameStartsWithVowel) "hasNeitherAnOrAnMethod" else "hasNeitherAOrAnMethod",
            left,
            UnquotedString(methodNameToInvoke),
            UnquotedString(methodNameToInvokeWithIs)
          )
        )

      case Some(result) =>

        val (wasNot, was) =
          if (hasArticle) {
            if (articleIsA) ("wasNotA", "wasA") else ("wasNotAn", "wasAn")
          }
          else ("wasNot", "was")

        MatchResult(
          result == true, // Right now I just leave the return value of accessProperty as Any
          FailureMessages(wasNot, left, UnquotedString(propertyName)),
          FailureMessages(was, left, UnquotedString(propertyName))
        )
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAnWordToBePropertyMatcherApplication[T](val bePropertyMatcher: BePropertyMatcher[T])
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLessThanOrEqualToComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be <= (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be <= (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left <= right
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfGreaterThanOrEqualToComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be >= (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be >= (10) and not be < (7))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left >= right
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLessThanComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be < (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be < (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left < right
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfGreaterThanComparison[T <% Ordered[T]](val right: T) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be > (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be > (10) and not be < (7))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: T): Boolean = left > right
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfTripleEqualsApplication(val right: Any) {

    /**
     * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
     * enables syntax such as:
     *
     * <pre class="stHighlight">
     * result should not be === (7)
     *                   ^  ... invoked by this be method
     * </pre>
     *
     * <p>
     * or
     * </p>
     *
     * <pre class="stHighlight">
     * num should (not be === (10) and not be > (17))
     *                 ^  ... invoked by this be method
     * </pre>
     */
    def apply(left: Any): Boolean = left == right
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAWordToBePropertyMatcherApplication[T](val bePropertyMatcher: BePropertyMatcher[T])
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfTheSameInstanceAsApplication(val right: AnyRef)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAnWordToSymbolApplication(val symbol: Symbol)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfAWordToSymbolApplication(val symbol: Symbol)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  /*sealed*/ class ResultOfNotWord[T](left: T, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not equal (7)
     *                   ^
     * </pre>
     */
    def equal(right: Any) {
      if ((left == right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
           if (shouldBeTrue) "didNotEqual" else "equaled",
            left,
            right
          )
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be (7)
     *                   ^
     * </pre>
     */
    def be(right: Any) {
      if ((left == right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
           if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
            left,
            right
          )
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be <= (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotLessThanOrEqualTo" else "wasLessThanOrEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be >= (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotGreaterThanOrEqualTo" else "wasGreaterThanOrEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be < (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotLessThan" else "wasLessThan",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be > (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotGreaterThan" else "wasGreaterThan",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be === (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfTripleEqualsApplication) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>odd</code> refers to
     * a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * 2 should not be (odd)
     *              ^
     * </pre>
     */
    def be(beMatcher: BeMatcher[T]) {
      val result = beMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            result.failureMessage
          else
            result.negatedFailureMessage
        )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  /*sealed*/ class ResultOfNotWordForAnyRef[T <: AnyRef](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWord[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not be (null)
     *                ^
     * </pre>
     */
    def be(o: Null) {
      if ((left == null) != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotNull", left) 
          else
            FailureMessages("wasNull")
        )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * stack should not be ('empty)
     *                  ^
     * </pre>
     */
    def be(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, false, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
     * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
     *
     * <pre class="stHighlight">
     * stack should not be (empty)
     *                      ^
     * </pre>
     */
    def be(bePropertyMatcher: BePropertyMatcher[T]) {
      val result = bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNot", left, UnquotedString(result.propertyName))
          else
            FailureMessages("was", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notFileMock should not be a ('file)
     *                        ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      val matcherResult = matchSymbolToPredicateMethod(left, resultOfAWordApplication.symbol, true, true)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * notFileMock should not be a (file)
     *                        ^
     * </pre>
     */
    def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]) {
      val result = resultOfAWordApplication.bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * keyEvent should not be an ('actionKey)
     *                     ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      val matcherResult = matchSymbolToPredicateMethod(left, resultOfAnWordApplication.symbol, true, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * keyEvent should not be an (actionKey)
     *                     ^
     * </pre>
     */
    def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]) {
      val result = resultOfAnWordApplication.bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasAn", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * otherString should not be theSameInstanceAs (string)
     *                        ^
     * </pre>
     */
    def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication) {
      if ((resultOfSameInstanceAsApplication.right eq left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
            left,
            resultOfSameInstanceAsApplication.right
          )
        )
      }
    }

    // TODO: Explain this matrix somewhere
    // The type parameter U has T as its lower bound, which means that U must be T or a supertype of T. Left is T, oh, because
    // HavePropertyMatcher is contravariant in its type parameter T, and that nmakes sense, because a HavePropertyMatcher of Any should
    // be able to match on a String.
    // <code>not have (a (1), b (2))</code> must mean the opposite of <code>have (a (1), b (2))</code>, which means that 
    // <code>not have (a (1), b (2))</code> will be true if either <code>(a (1)).matches</code> or <code>(b (1)).matches</code> is false.
    // Only if both <code>(a (1)).matches</code> or <code>(b (1)).matches</code> are true will <code>not have (a (1), b (2))</code> be false.
    // title/author matches | have | have not
    // 0 0 | 0 | 1 
    // 0 1 | 0 | 1
    // 1 0 | 0 | 1
    // 1 1 | 1 | 0
    // 
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>title ("One Hundred Years of Solitude")</code> results in a <code>HavePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * book should not have (title ("One Hundred Years of Solitude"))
     *                 ^
     * </pre>
     */
    def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*) {

      val results =
        for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
          propertyVerifier(left)

      val firstFailureOption = results.find(pv => !pv.matches)

      val justOneProperty = propertyMatchers.length == 0

      // if shouldBeTrue is false, then it is like "not have ()", and should throw TFE if firstFailureOption.isDefined is false
      // if shouldBeTrue is true, then it is like "not (not have ()), which should behave like have ()", and should throw TFE if firstFailureOption.isDefined is true
      if (firstFailureOption.isDefined == shouldBeTrue) {
        firstFailureOption match {
          case Some(firstFailure) =>
            // This is one of these cases, thus will only get here if shouldBeTrue is true
            // 0 0 | 0 | 1
            // 0 1 | 0 | 1
            // 1 0 | 0 | 1
            throw newTestFailedException(
              FailureMessages(
                "propertyDidNotHaveExpectedValue",
                 UnquotedString(firstFailure.propertyName),
                 firstFailure.expectedValue,
                 firstFailure.actualValue,
                 left
              )
            )
          case None =>
            // This is this cases, thus will only get here if shouldBeTrue is false
            // 1 1 | 1 | 0
            val failureMessage =
              if (justOneProperty) {
                val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                FailureMessages(
                  "propertyHadExpectedValue",
                  UnquotedString(firstPropertyResult.propertyName),
                  firstPropertyResult.expectedValue,
                  left
                )
              }
              else FailureMessages("allPropertiesHadExpectedValues", left)

            throw newTestFailedException(failureMessage)
        } 
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfBeWordForAnyRef[T <: AnyRef](left: T, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be theSameInstanceAs anotherObject
     *                  ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef) {
      if ((left eq right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
            left,
            right
          )
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fileMock should be a ('file)
     *                    ^
     * </pre>
     */
    def a(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, true, true)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    // TODO: Check the shouldBeTrues, are they sometimes always false or true?
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should be a (goodRead)
     *                   ^
     * </pre>
     */
    def a(bePropertyMatcher: BePropertyMatcher[T]) {
      val result = bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
        )
      }
    }

    // TODO, in both of these, the failure message doesn't have a/an
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fruit should be an ('orange)
     *                 ^
     * </pre>
     */
    def an(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, true, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * book should be an (excellentRead)
     *                ^
     * </pre>
     */
    def an(beTrueMatcher: BePropertyMatcher[T]) {
      val beTrueMatchResult = beTrueMatcher(left)
      if (beTrueMatchResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAn", left, UnquotedString(beTrueMatchResult.propertyName))
          else
            FailureMessages("wasAn", left, UnquotedString(beTrueMatchResult.propertyName))
        )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfLengthWordApplication(val expectedLength: Long) extends HavePropertyMatcher[AnyRef, Long] {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "hi" should not have (length (3))
     *                      ^
     * </pre>
     *
     * <p>
     * This reason <code>ResultOfLengthWordApplication</code> is a <code>HavePropertyMatcher[AnyRef, Long]</code> is
     * so that you don't have to remember whether <code>length</code> needs to be surrounded by parentheses when following
     * <code>have</code>. Only <code>length</code> and <code>size</code> can be used without parentheses: everything else
     * needs the parentheses. So this approach means that if you use the unneeded parentheses with <code>length</code> and
     * <code>size</code>, it will still work. This <code>apply</code> method uses reflection to find and access the <code>length</code>
     * property on the passed <code>objectWithProperty</code>. Therefore if the object does not have the appropriate structure, the expression
     * will compile, but at will produce a <code>TestFailedException</code> at runtime.
     * </p>
     */
    def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Long] = {

      accessProperty(objectWithProperty, 'length, false) match {

        case None =>

          throw newTestFailedException(Resources("propertyNotFound", "length", expectedLength.toString, "getLength"))

        case Some(result) =>

          new HavePropertyMatchResult[Long](
            result == expectedLength,
            "length",
            expectedLength,
            result match {
              case value: Byte => value.toLong
              case value: Short => value.toLong
              case value: Int => value.toLong
              case value: Long => value
              case _ => throw newTestFailedException(Resources("lengthPropertyNotAnInteger"))
            }
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfSizeWordApplication(val expectedSize: Long) extends HavePropertyMatcher[AnyRef, Long] {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should not have (size (3))
     *                     ^
     * </pre>
     *
     * <p>
     * This reason <code>ResultOfSizeWordApplication</code> is a <code>HavePropertyMatcher[AnyRef, Long]</code> is
     * so that you don't have to remember whether <code>size</code> needs to be surrounded by parentheses when following
     * <code>have</code>. Only <code>length</code> and <code>size</code> can be used without parentheses: everything else
     * needs the parentheses. So this approach means that if you use the unneeded parentheses with <code>length</code> and
     * <code>size</code>, it will still work. This <code>apply</code> method uses reflection to find and access the <code>size</code>
     * property on the passed <code>objectWithProperty</code>. Therefore if the object does not have the appropriate structure, the expression
     * will compile, but at will produce a <code>TestFailedException</code> at runtime.
     * </p>
     */
    def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Long] = {

      accessProperty(objectWithProperty, 'size, false) match {

        case None =>

          throw newTestFailedException(Resources("propertyNotFound", "size", expectedSize.toString, "getSize"))

        case Some(result) =>

          new HavePropertyMatchResult[Long](
            result == expectedSize,
            "size",
            expectedSize,
            result match {
              case value: Byte => value.toLong
              case value: Short => value.toLong
              case value: Int => value.toLong
              case value: Long => value
              case _ => throw newTestFailedException(Resources("sizePropertyNotAnInteger"))
            }
          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfValueWordApplication[T](val expectedValue: T)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfKeyWordApplication[T](val expectedKey: T)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should equal (7)
   *               ^
   * </pre>
   *
   * <p>
   * The <code>left should equal (right)</code> syntax works by calling <code>==</code> on the <code>left</code>
   * value, passing in the <code>right</code> value, on every type except arrays. If both <code>left</code> and right are arrays, <code>deep</code>
   * will be invoked on both <code>left</code> and <code>right</code> before comparing them with <em>==</em>. Thus, even though this expression
   * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
   * </p>
   * 
   * <pre class="stHighlight">
   * Array(1, 2) == Array(1, 2) // yields false
   * </pre>
   *
   * <p>
   * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because ScalaTest will compare
   * the two arrays structurally, taking into consideration the equality of the array's contents:
   * </p>
   *
   * <pre class="stHighlight">
   * Array(1, 2) should equal (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
   * </pre>
   *
   * <p>
   * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
   * <code>be theSameInstanceAs</code> syntax.
   * </p>
   *
   */
  def equal(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
          MatchResult(
            areEqualComparingArraysStructurally(left, right),
            FailureMessages("didNotEqual", leftee, rightee),
            FailureMessages("equaled", left, right)
          )
        }
      }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * Class <code>BeWord</code> contains an <code>apply</code> method that takes a <code>Symbol</code>, which uses reflection
   * to find and access a <code>Boolean</code> property and determine if it is <code>true</code>.
   * If the symbol passed is <code>'empty</code>, for example, the <code>apply</code> method
   * will use reflection to look for a public Java field named
   * "empty", a public method named "empty", or a public method named "isEmpty". If a field, it must be of type <code>Boolean</code>.
   * If a method, it must take no parameters and return <code>Boolean</code>. If multiple candidates are found,
   * the <code>apply</code> method will select based on the following algorithm:
   * </p>
   * 
   * <table class="stTable">
   * <tr><th class="stHeadingCell">Field</th><th class="stHeadingCell">Method</th><th class="stHeadingCell">"is" Method</th><th class="stHeadingCell">Result</th></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Throws <code>TestFailedException</code>, because no candidates found</td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Accesses field <code>empty</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
   * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * </table>
   * 
   * @author Bill Venners
   */
  final class BeWord {


    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &lt; (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the less than operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the less than operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &lt; (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &lt; (7))
     *                       ^
     * </pre>
     */
    def <[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left < right,
            FailureMessages("wasNotLessThan", left, right),
            FailureMessages("wasLessThan", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &gt; (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the greater than operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the greater than operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &gt; (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &gt; (7))
     *                       ^
     * </pre>
     */
    def >[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left > right,
            FailureMessages("wasNotGreaterThan", left, right),
            FailureMessages("wasGreaterThan", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &lt;= (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the less than or equal to operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the less than or equal to operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &lt;= (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &lt;= (7))
     *                       ^
     * </pre>
     */
    def <=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left <= right,
            FailureMessages("wasNotLessThanOrEqualTo", left, right),
            FailureMessages("wasLessThanOrEqualTo", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be &gt;= (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the greater than or equal to operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the greater than or equal to operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be &gt;= (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be &gt;= (7))
     *                       ^
     * </pre>
     */
    def >=[T <% Ordered[T]](right: T): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            left >= right,
            FailureMessages("wasNotGreaterThanOrEqualTo", left, right),
            FailureMessages("wasGreaterThanOrEqualTo", left, right)
          )
      }


    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be === (7)
     *                  ^
     * </pre>
     *
     * <p>
     * Note that the === operator will be invoked on <code>be</code> in this expression, not
     * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
     * in the matchers DSL, because the ===n operator has a higher precedence than <code>should</code>.
     * Thus in the above case the first expression evaluated will be <code>be === (7)</code>, which results
     * in a matcher that is passed to <code>should</code>.
     * </p>
     *
     * <p>
     * This method also enables the following syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * result should not (be === (7))
     *                       ^
     * </pre>
     */
    def ===(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
          MatchResult(
            areEqualComparingArraysStructurally(left, right),
            FailureMessages("wasNotEqualTo", leftee, rightee),
            FailureMessages("wasEqualTo", left, right)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * fileMock should not { be a ('file) }
     *                          ^
     * </pre>
     */
    def a[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, true, true)
      }

    /**
     * This method enables the following syntax, where <code>fileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * fileMock should not { be a (file) }
     *                          ^
     * </pre>
     */
    def a[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName)), 
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * animal should not { be an ('elephant) }
     *                        ^
     * </pre>
     */
    def an[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, true, false)
      }

    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * keyEvent should not { be an (actionKey) }
     *                          ^
     * </pre>
     */
    def an[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName)),
            FailureMessages("wasAn", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should be (7.1 plusOrMinus 0.2)
     *                      ^
     * </pre>
     */
    def apply(doubleTolerance: DoubleTolerance): Matcher[Double] =
      new Matcher[Double] {
        def apply(left: Double): MatchResult = {
          import doubleTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhFloat should be (7.1f plusOrMinus 0.2f)
     *                           ^
     * </pre>
     */
    def apply(floatTolerance: FloatTolerance): Matcher[Float] =
      new Matcher[Float] {
        def apply(left: Float): MatchResult = {
          import floatTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenLong should be (7L plusOrMinus 2L)
     *                     ^
     * </pre>
     */
    def apply(longTolerance: LongTolerance): Matcher[Long] =
      new Matcher[Long] {
        def apply(left: Long): MatchResult = {
          import longTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenInt should be (7 plusOrMinus 2)
     *                     ^
     * </pre>
     */
    def apply(intTolerance: IntTolerance): Matcher[Int] =
      new Matcher[Int] {
        def apply(left: Int): MatchResult = {
          import intTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenShort should be (7.toShort plusOrMinus 2.toShort)
     *                     ^
     * </pre>
     */
    def apply(shortTolerance: ShortTolerance): Matcher[Short] =
      new Matcher[Short] {
        def apply(left: Short): MatchResult = {
          import shortTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenByte should be (7.toByte plusOrMinus 2.toByte)
     *                     ^
     * </pre>
     */
    def apply(byteTolerance: ByteTolerance): Matcher[Byte] =
      new Matcher[Byte] {
        def apply(left: Byte): MatchResult = {
          import byteTolerance._
          MatchResult(
            left <= right + tolerance && left >= right - tolerance,
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance)
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be theSameInstancreAs (anotherObject)
     *                  ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          MatchResult(
            left eq right,
            FailureMessages("wasNotSameInstanceAs", left, right),
            FailureMessages("wasSameInstanceAs", left, right)
          )
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be (true)
     *                  ^
     * </pre>
     */
    def apply(right: Boolean): Matcher[Boolean] = 
      new Matcher[Boolean] {
        def apply(left: Boolean): MatchResult =
          MatchResult(
            left == right,
            FailureMessages("wasNot", left, right),
            FailureMessages("was", left, right)
          )
      }

/* Well heck if I don't need this one
      [fsc] both method apply in class BeWord of type [T](org.scalatest.BePropertyMatcher[T])org.scalatest.Matcher[T]
      [fsc] and  method apply in class BeWord of type [T](org.scalatest.BeMatcher[T])org.scalatest.Matcher[T]
      [fsc] match argument types (Null)
      [fsc]         o should be (null)
      [fsc]                  ^
*/

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should be (null)
     *                  ^
     * </pre>
     */
    def apply(o: Null): Matcher[AnyRef] = 
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult = {
          MatchResult(
            left == null,
            FailureMessages("wasNotNull", left),
            FailureMessages("wasNull"),
            FailureMessages("wasNotNull", left),
            FailureMessages("midSentenceWasNull")
          )
        }
      }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should be ('empty)
     *               ^
     * </pre>
     */
    def apply[S <: AnyRef](right: Symbol): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, false, false)
      }

    /**
     * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
     * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * num should be (odd)
     *               ^
     * </pre>
     */
    def apply[T](right: BeMatcher[T]): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult = right(left)
      }

    /**
     * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
     *
     * <pre class="stHighlight">
     * door should be (open)
     *                ^
     * </pre>
     */
    def apply[T](bePropertyMatcher: BePropertyMatcher[T]): Matcher[T] =
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            result.matches,
            FailureMessages("wasNot", left, UnquotedString(result.propertyName)), 
            FailureMessages("was", left, UnquotedString(result.propertyName))
          )
        }
      }

    /**
     * This method enables <code>be</code> to be used for equality comparison. Here are some examples: 
     *
     * <pre class="stHighlight">
     * result should be (None)
     *                  ^
     * result should be (Some(1))
     *                  ^
     * result should be (true)
     *                  ^
     * result should be (false)
     *                  ^
     * sum should be (19)
     *               ^
     * </pre>
     */
    def apply(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult =
          left match {
            case null =>
              MatchResult(
                right == null,
                FailureMessages("wasNotNull", right),
                FailureMessages("wasNull"),
                FailureMessages("wasNotNull", right),
                FailureMessages("midSentenceWasNull")
              )
            case _ => {
              val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
              MatchResult(
                areEqualComparingArraysStructurally(left, right),
                FailureMessages("wasNotEqualTo", leftee, rightee),
                FailureMessages("wasEqualTo", left, right)
              )
            }
        }
      }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class HaveWord {

    // TODO: How about returning a Matcher[Gazornimplatz] and then providing implicit conversion
    // methods from Matcher[Gazornimplatz] to Matcher[Seq], Matcher[String], Matcher[java.util.List], and
    // Matcher[the structural length methods]. This is similar to the technique I used with "contain (7)"
    // to get it to work with java.util.Collection.
    // I couldn't figure out how to combine view bounds with existential types. May or may not
    // be possible, but going dynamic for now at least.
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have length (9)
     *                  ^
     * </pre>
     *
     * <p>
     * Currently (as of ScalaTest 0.9.5), this method will produce a <code>Matcher[AnyRef]</code>, and if the
     * <code>AnyRef</code> passed to that matcher's <code>apply</code> method does not have the appropriate <code>length</code> property
     * structure, all will compile but a <code>TestFailedException</code> will result at runtime explaining the problem. The one exception is that it will work on
     * <code>java.util.List</code>, even though that type has no <code>length</code> structure (its <code>size</code> property
     * will be used instead.) In a future ScalaTest release, this may be tightened so that all is statically checked at compile time.
     * </p>
     */
    def length(expectedLength: Long): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          left match {
            case leftArray: Array[_] =>
              MatchResult(
                leftArray.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftSeq: Seq[_] =>
              MatchResult(
                leftSeq.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftString: String =>
              MatchResult(
                leftString.length == expectedLength, 
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case leftJavaList: java.util.List[_] =>
              MatchResult(
                leftJavaList.size == expectedLength,
                FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                FailureMessages("hadExpectedLength", left, expectedLength)
              )
            case _ =>

              accessProperty(left, 'length, false) match {

                case None =>

                  throw newTestFailedException(Resources("noLengthStructure", expectedLength.toString))

                case Some(result) =>

                  MatchResult(
                    result == expectedLength,
                    FailureMessages("didNotHaveExpectedLength", left, expectedLength),
                    FailureMessages("hadExpectedLength", left, expectedLength)
                  )
              }
          }
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have size (9)
     *                  ^
     * </pre>
     *
     * <p>
     * Currently, this method will produce a <code>Matcher[AnyRef]</code>, and if the
     * <code>AnyRef</code> passed to that matcher's <code>apply</code> method does not have the appropriate <code>size</code> property
     * structure, all will compile but a <code>TestFailedException</code> will result at runtime explaining the problem.
     * In a future ScalaTest release, this may be tightened so that all is statically checked at compile time.
     * </p>
     */
    def size(expectedSize: Long): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult =
          left match {
            case leftArray: Array[_] =>
              MatchResult(
                leftArray.length == expectedSize, 
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case leftTrav: Traversable[_] =>
              MatchResult(
                leftTrav.size == expectedSize, 
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case leftJavaList: java.util.List[_] =>
              MatchResult(
                leftJavaList.size == expectedSize,
                FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                FailureMessages("hadExpectedSize", left, expectedSize)
              )
            case _ =>

              accessProperty(left, 'size, false) match {

                case None =>

                  throw newTestFailedException(Resources("noSizeStructure", expectedSize.toString))

                case Some(result) =>

                  MatchResult(
                    result == expectedSize,
                    FailureMessages("didNotHaveExpectedSize", left, expectedSize),
                    FailureMessages("hadExpectedSize", left, expectedSize)
                  )
              }
          }
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have (title ("A Tale of Two Cities"))
     *                  ^
     * </pre>
     */
    def apply[T](firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Matcher[T] =

      new Matcher[T] {

        def apply(left: T): MatchResult = {

          val results =
            for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
              propertyVerifier(left)

          val firstFailureOption = results.find(pv => !pv.matches)

          val justOneProperty = propertyMatchers.length == 0

          firstFailureOption match {

            case Some(firstFailure) =>

              val failedVerification = firstFailure
              val failureMessage =
                FailureMessages(
                  "propertyDidNotHaveExpectedValue",
                  UnquotedString(failedVerification.propertyName),
                  failedVerification.expectedValue,
                  failedVerification.actualValue,
                  left
                )
              val midSentenceFailureMessage =
                FailureMessages(
                  "midSentencePropertyDidNotHaveExpectedValue",
                  UnquotedString(failedVerification.propertyName),
                  failedVerification.expectedValue,
                  failedVerification.actualValue,
                  left
                )

              MatchResult(false, failureMessage, failureMessage, midSentenceFailureMessage, midSentenceFailureMessage)

            case None =>

              val failureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages(
                    "propertyHadExpectedValue",
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    left
                  )
                }
                else FailureMessages("allPropertiesHadExpectedValues", left)

              val midSentenceFailureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages(
                    "midSentencePropertyHadExpectedValue",
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    left
                  )
                }
                else FailureMessages("midSentenceAllPropertiesHadExpectedValues", left)

              MatchResult(true, failureMessage, failureMessage, midSentenceFailureMessage, midSentenceFailureMessage)
          }
        }
      }
  }
  
  /*
    In HaveWord's methods key, value, length, and size, I can give type parameters.
    The type HaveWord can contain a key method that takes a S or what not, and returns a matcher, which
    stores the key value in a val and whose apply method checks the passed map for the remembered key. This one would be used in things like:

    map should { have key 9 and have value "bob" }

    There's an overloaded should method on Shouldifier that takes a HaveWord. This method results in
    a different type that also has a key method that takes an S. So when you say:

    map should have key 9

    what happens is that this alternate should method gets invoked. The result is this other class that
    has a key method, and its constructor takes the map and stores it in a val. So this time when key is
    invoked, it checks to make sure the passed key is in the remembered map, and does the assertion.

    length and size can probably use structural types, because I want to use length on string and array for
    starters, and other people may create classes that have length methods. Would be nice to be able to use them.
  */
  
  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (have length (3) and not contain ('a'))
   *              ^
   * </pre>
   */
  val have = new HaveWord
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class NotWord {

    /**
     * This method enables the following syntax, where <code>tempFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>exist</code> is a <code>Matcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * tempFile should not (exist)
     *                     ^
     * </pre>
     */
    def apply[S <: Any](matcher: Matcher[S]): Matcher[S] =
      new Matcher[S] {
        def apply(left: S): MatchResult =
          matcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }

    /**
     * This method enables any <code>BeMatcher</code> to be negated by passing it to <code>not</code>. 
     * For example, if you have a <code>BeMatcher[Int]</code> called <code>odd</code>, which matches
     * <code>Int</code>s that are odd, you can negate it to get a <code>BeMatcher[Int]</code> that matches
     * even <code>Int</code>s, like this:
     *
     * <pre class="stHighlight">
     * val even = not (odd)
     *                ^
     * </pre>
     *
     * <p>
     * In addition, this method enables you to negate a <code>BeMatcher</code> at its point of use, like this:
     * </p>
     *
     * <pre class="stHighlight">
     * num should be (not (odd))
     * </pre>
     *
     * <p>
     * Nevertheless, in such as case it would be more idiomatic to write:
     * </p>
     *
     * <pre class="stHighlight">
     * num should not be (odd)
     * </pre>
     */
    def apply[S <: Any](beMatcher: BeMatcher[S]): BeMatcher[S] =
      new BeMatcher[S] {
        def apply(left: S): MatchResult =
          beMatcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * num should (not equal (7) and be < (9))
     *                 ^
     * </pre>
     */
    def equal(right: Any): Matcher[Any] = apply(matchers.equal(right))

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have length (5) and not have length (3))
     *                         ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): Matcher[AnyRef] =
      apply(matchers.have.length(resultOfLengthWordApplication.expectedLength))

    // This looks similar to the AndNotWord one, but not quite the same because no and
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (5) and not have size (3))
     *                         ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): Matcher[AnyRef] =
      apply(matchers.have.size(resultOfSizeWordApplication.expectedSize))

    /**
     * This method enables the following syntax, where, for example, <code>book</code> is of type <code>Book</code> and <code>title</code> and <code>author</code>
     * are both of type <code>HavePropertyMatcher[Book, String]</code>:
     *
     * <pre class="stHighlight">
     * book should (not have (title ("Moby Dick")) and (not have (author ("Melville"))))
     *                  ^
     * </pre>
     */
    def have[T](firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*): Matcher[T] =
      apply(matchers.have(firstPropertyMatcher, propertyMatchers: _*))

    /**
     * This method enables the following syntax, where, for example, <code>num</code> is an <code>Int</code> and <code>odd</code>
     * of type <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * num should (not be (odd) and be <= (8))
     *                 ^
     * </pre>
     */
    def be[T](beMatcher: BeMatcher[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          beMatcher(left) match {
            case MatchResult(bool, s1, s2, s3, s4) => MatchResult(!bool, s2, s1, s4, s3)
          }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should (not be (null))
     *                 ^
     * </pre>
     */
    def be(o: Null): Matcher[AnyRef] =
      new Matcher[AnyRef] {
        def apply(left: AnyRef): MatchResult = {
          MatchResult(
            left != null,
            FailureMessages("wasNull"),
            FailureMessages("wasNotNull", left),
            FailureMessages("midSentenceWasNull"),
            FailureMessages("wasNotNull", left)
          )
        }
      }

    // These next four are for things like not be </>/<=/>=:
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be < (7) and not be > (10))
     *                 ^
     * </pre>
     */
    def be[T](resultOfLessThanComparison: ResultOfLessThanComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfLessThanComparison(left),
            FailureMessages("wasLessThan", left, resultOfLessThanComparison.right),
            FailureMessages("wasNotLessThan", left, resultOfLessThanComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be > (10) and not be < (7))
     *                 ^
     * </pre>
     */
    def be[T](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfGreaterThanComparison(left),
            FailureMessages("wasGreaterThan", left, resultOfGreaterThanComparison.right),
            FailureMessages("wasNotGreaterThan", left, resultOfGreaterThanComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be <= (7) and not be > (10))
     *                 ^
     * </pre>
     */
    def be[T](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfLessThanOrEqualToComparison(left),
            FailureMessages("wasLessThanOrEqualTo", left, resultOfLessThanOrEqualToComparison.right),
            FailureMessages("wasNotLessThanOrEqualTo", left, resultOfLessThanOrEqualToComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * num should (not be >= (10) and not be < (7))
     *                 ^
     * </pre>
     */
    def be[T](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult =
          MatchResult(
            !resultOfGreaterThanOrEqualToComparison(left),
            FailureMessages("wasGreaterThanOrEqualTo", left, resultOfGreaterThanOrEqualToComparison.right),
            FailureMessages("wasNotGreaterThanOrEqualTo", left, resultOfGreaterThanOrEqualToComparison.right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * num should (not be === (7) and not be === (10))
     *                 ^
     * </pre>
     */
    def be(resultOfTripleEqualsApplication: ResultOfTripleEqualsApplication): Matcher[Any] = {
      new Matcher[Any] {
        def apply(left: Any): MatchResult =
          MatchResult(
            !resultOfTripleEqualsApplication(left),
            FailureMessages("wasEqualTo", left, resultOfTripleEqualsApplication.right),
            FailureMessages("wasNotEqualTo", left, resultOfTripleEqualsApplication.right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFile should (not be ('hidden) and have (name ("temp.txt")))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](symbol: Symbol): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, symbol, false, false)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>tempFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>hidden</code> is a <code>BePropertyMatcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * tempFile should (not be (hidden) and have ('name ("temp.txt")))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](bePropertyMatcher: BePropertyMatcher[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("was", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNot", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * isNotFileMock should (not be a ('file) and have ('name ("temp.txt"))))
     *                           ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, resultOfAWordApplication.symbol, true, true)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax, where <code>notSoSecretFile</code>, for example, refers to a <code>java.io.File</code>
     * and <code>directory</code> is a <code>BePropertyMatcher[java.io.File]</code>: 
     *
     * <pre class="stHighlight">
     * notSoSecretFile should (not be a (directory) and have ('name ("passwords.txt")))
     *                             ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = resultOfAWordApplication.bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("wasA", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * isNotAppleMock should (not be an ('apple) and not be ('rotten))
     *                            ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val positiveMatchResult = matchSymbolToPredicateMethod(left, resultOfAnWordApplication.symbol, true, false)
          MatchResult(
            !positiveMatchResult.matches,
            positiveMatchResult.negatedFailureMessage,
            positiveMatchResult.failureMessage
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFile should (not be an (directory) and not be an (directory))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T]): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          val result = resultOfAnWordApplication.bePropertyMatcher(left)
          MatchResult(
            !result.matches,
            FailureMessages("wasAn", left, UnquotedString(result.propertyName)),
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName))
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * myFish should (not be theSameInstanceAs (redFish) and not be theSameInstanceAs (blueFish))
     *                    ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T] = {
      new Matcher[T] {
        def apply(left: T): MatchResult = {
          MatchResult(
            resultOfTheSameInstanceAsApplication.right ne left,
            FailureMessages("wasSameInstanceAs", left, resultOfTheSameInstanceAsApplication.right),
            FailureMessages("wasNotSameInstanceAs", left, resultOfTheSameInstanceAsApplication.right)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should ((not be (17.1 plusOrMinus 0.2)) and (not be (27.1 plusOrMinus 0.2)))
     *                         ^
     * </pre>
     */
    def be(doubleTolerance: DoubleTolerance): Matcher[Double] = {
      import doubleTolerance._
      new Matcher[Double] {
        def apply(left: Double): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOhFloat should ((not be (17.1f plusOrMinus 0.2f)) and (not be (27.1f plusOrMinus 0.2f)))
     *                         ^
     * </pre>
     */
    def be(floatTolerance: FloatTolerance): Matcher[Float] = {
      import floatTolerance._
      new Matcher[Float] {
        def apply(left: Float): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenLong should ((not be (19L plusOrMinus 2L)) and (not be (29L plusOrMinus 2L)))
     *                        ^
     * </pre>
     */
    def be(longTolerance: LongTolerance): Matcher[Long] = {
      import longTolerance._
      new Matcher[Long] {
        def apply(left: Long): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenInt should ((not be (19 plusOrMinus 2)) and (not be (29 plusOrMinus 2)))
     *                       ^
     * </pre>
     */
    def be(intTolerance: IntTolerance): Matcher[Int] = {
      import intTolerance._
      new Matcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenShort should ((not be (19.toShort plusOrMinus 2.toShort)) and (not be (29.toShort plusOrMinus 2.toShort)))
     *                         ^
     * </pre>
     */
    def be(shortTolerance: ShortTolerance): Matcher[Short] = {
      import shortTolerance._
      new Matcher[Short] {
        def apply(left: Short): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenByte should ((not be (19.toByte plusOrMinus 2.toByte)) and (not be (29.toByte plusOrMinus 2.toByte)))
     *                        ^
     * </pre>
     */
    def be(byteTolerance: ByteTolerance): Matcher[Byte] = {
      import byteTolerance._
      new Matcher[Byte] {
        def apply(left: Byte): MatchResult = {
          MatchResult(
            !(left <= right + tolerance && left >= right - tolerance),
            FailureMessages("wasPlusOrMinus", left, right, tolerance),
            FailureMessages("wasNotPlusOrMinus", left, right, tolerance)
          )
        }
      }
    }

    /**
     * This method enables <code>be</code> to be used for inequality comparison. Here are some examples:
     *
     * <pre class="stHighlight">
     * result should not be (None)
     *                      ^
     * result should not be (Some(1))
     *                      ^
     * result should not be (true)
     *                      ^
     * result should not be (false)
     *                      ^
     * sum should not be (19)
     *                   ^
     * </pre>
     */
    def be(right: Any): Matcher[Any] = {
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          left match {
            case null =>
              MatchResult(
                right != null, 
                FailureMessages("wasNull"),
                FailureMessages("wasNotNull", right),
                FailureMessages("midSentenceWasNull"),
                FailureMessages("wasNotNull", right)
              )
            case _ => 
              MatchResult(
                !areEqualComparingArraysStructurally(left, right),
                FailureMessages("wasEqualTo", left, right),
                FailureMessages("wasNotEqualTo", left, right)
              )
          }
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not fullyMatch regex ("Hel*o") and not include ("orld"))
     *                    ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegexString = resultOfRegexWordApplication.regex.toString
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !java.util.regex.Pattern.matches(rightRegexString, left),
            FailureMessages("fullyMatchedRegex", left, UnquotedString(rightRegexString)),
            FailureMessages("didNotFullyMatchRegex", left, UnquotedString(rightRegexString))
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not include regex ("Hel.o") and not include regex ("""(-)?(\d+)(\.\d*)?"""))
     *                    ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !rightRegex.findFirstIn(left).isDefined,
            FailureMessages("includedRegex", left, rightRegex),
            FailureMessages("didNotIncludeRegex", left, rightRegex)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not include ("cat") and not include ("1.7"))
     *                    ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !(left.indexOf(expectedSubstring) >= 0), 
            FailureMessages("includedSubstring", left, expectedSubstring),
            FailureMessages("didNotIncludeSubstring", left, expectedSubstring)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not startWith regex ("hel*o") and not endWith regex ("wor.d"))
     *                    ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            !rightRegex.pattern.matcher(left).lookingAt,
            FailureMessages("startedWithRegex", left, rightRegex),
            FailureMessages("didNotStartWithRegex", left, rightRegex)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should ((not startWith ("red")) and (not startWith ("1.7")))
     *                     ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult =
          MatchResult(
            left.indexOf(expectedSubstring) != 0,
            FailureMessages("startedWith", left, expectedSubstring),
            FailureMessages("didNotStartWith", left, expectedSubstring)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not endWith regex ("wor.d") and not startWith regex ("Hel*o"))
     *                    ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[String] = {
      val rightRegex = resultOfRegexWordApplication.regex
      new Matcher[String] {
        def apply(left: String): MatchResult = {
          val allMatches = rightRegex.findAllIn(left)
          MatchResult(
            !(allMatches.hasNext && (allMatches.end == left.length)),
            FailureMessages("endedWithRegex", left, rightRegex),
            FailureMessages("didNotEndWithRegex", left, rightRegex)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should (not endWith ("blue") and not endWith ("1.7"))
     *                    ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[String] = {
      new Matcher[String] {
        def apply(left: String): MatchResult = {
          MatchResult(
            !(left endsWith expectedSubstring),
            FailureMessages("endedWith", left, expectedSubstring),
            FailureMessages("didNotEndWith", left, expectedSubstring)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not contain (5) and not contain (3))
     *                         ^
     * </pre>
     */
    def contain[T](expectedElement: T): Matcher[Traversable[T]] = {
      new Matcher[Traversable[T]] {
        def apply(left: Traversable[T]): MatchResult = {
          MatchResult(
            !(left.exists(_ == expectedElement)),
            FailureMessages("containedExpectedElement", left, expectedElement),
            FailureMessages("didNotContainExpectedElement", left, expectedElement)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain key ("three"))
     *                                         ^
     * </pre>
     */
    def contain[K](resultOfKeyWordApplication: ResultOfKeyWordApplication[K]): Matcher[scala.collection.Map[K, Any]] = {
      val expectedKey = resultOfKeyWordApplication.expectedKey
      new Matcher[scala.collection.Map[K, Any]] {
        def apply(left: scala.collection.Map[K, Any]): MatchResult = {
          MatchResult(
            !(left.contains(expectedKey)),
            FailureMessages("containedKey", left, expectedKey),
            FailureMessages("didNotContainKey", left, expectedKey)
          )
        }
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should (not contain value (3))
     *                                         ^
     * </pre>
     */
    def contain[K, V](resultOfValueWordApplication: ResultOfValueWordApplication[V]): Matcher[scala.collection.Map[K, V] forSome { type K }] = {
      val expectedValue = resultOfValueWordApplication.expectedValue
      new Matcher[scala.collection.Map[K, V] forSome { type K }] {
        def apply(left: scala.collection.Map[K, V] forSome { type K }): MatchResult = {
          MatchResult(
            !(left.values.exists(_ == expectedValue)),
            FailureMessages("containedValue", left, expectedValue),
            FailureMessages("didNotContainValue", left, expectedValue)
          )
        }
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * The primary constructor enables the following syntax (with a passed <code>scala.util.matching.Regex</code>): 
   * </p>
   *
   * <pre class="stHighlight">
   * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
   *                               ^
   * </pre>
   *
   * @author Bill Venners
   */
  final class ResultOfRegexWordApplication(val regex: Regex) {

    /**
     * This auxiliary constructor enables the following syntax (with a passed <code>java.lang.String</code>): 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                               ^
     * </pre>
     */
    def this(regexString: String) = this(new Regex(regexString))
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class ByteTolerance(right: Byte, tolerance: Byte)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class FloatTolerance(right: Float, tolerance: Float)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class LongTolerance(right: Long, tolerance: Long)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class IntTolerance(right: Int, tolerance: Int)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class ShortTolerance(right: Short, tolerance: Short)
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final case class DoubleTolerance(right: Double, tolerance: Double)
}