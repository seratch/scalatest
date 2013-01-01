package org.scalatest.matchers

import org.scalatest._
import collection.GenSeq

trait NounMatcher[T] extends Matcher[T] { thisNounMatcher =>
  val noun: String
  def apply(left: T): MatchResult
}

object NounMatcher {
  
  /**
   * Factory method that creates a <code>NounMatcher[T]</code> from a
   * passed function of type <code>(T => MatchResult)</code>.
   *
   */
  def apply[T](nounText: String)(fun: T => Boolean): NounMatcher[T] =
    new NounMatcher[T] {
      val noun = nounText
      def apply(left: T) = {
        MatchResult(
          fun(left), 
          FailureMessages(if (useAn(noun)) "shouldBeAnNoun" else "shouldBeANoun", left, noun),
          FailureMessages(if (useAn(noun)) "shouldNotBeAnNoun" else "shouldNotBeANoun", left, noun)
        )
      }
    }  
  
  private def useAn(noun: String) = 
    noun.startsWith("a") || noun.startsWith("e") || noun.startsWith("i") || noun.startsWith("o") || noun.startsWith("u")
}