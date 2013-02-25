package org.scalatest

import collection.GenTraversable
import org.scalautils.Equality

trait ContainMatcher[T] extends Function2[GenTraversable[T], Equality[T], MatchResult] {
  
  def apply(left: GenTraversable[T], equality: Equality[T]): MatchResult
  
}
