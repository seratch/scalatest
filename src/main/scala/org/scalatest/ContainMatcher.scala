package org.scalatest

import collection.GenTraversable
import org.scalautils.Equality

trait ContainMatcher[T] /*extends Function1[GenTraversable[T], Function1[Equality[GenTraversable[T]], MatchResult]]*/ {
  
  def apply(left: GenTraversable[T])(implicit equality: Equality[GenTraversable[T]]): MatchResult
  
}
