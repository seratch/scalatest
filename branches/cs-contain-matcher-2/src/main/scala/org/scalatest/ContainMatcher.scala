package org.scalatest

import collection.GenTraversable
import matchers.MatchResult
import matchers.Matcher

trait ContainMatcher[T] extends Matcher[GenTraversable[T]] {
  
  def apply(left: GenTraversable[T]): MatchResult
  
}
