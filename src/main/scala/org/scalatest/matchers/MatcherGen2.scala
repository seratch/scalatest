/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.matchers

import org.scalatest.Matchers.andMatchersAndApply
import org.scalatest.Matchers.orMatchersAndApply

abstract class MatcherGen2[-SUPERCLASS, TYPECLASS1[_], TYPECLASS2[_]] { thisMatcherGen2 =>

  def matcher[T <: SUPERCLASS : TYPECLASS1 : TYPECLASS2]: Matcher[T]

  def apply[T <: SUPERCLASS](explicit1: TYPECLASS1[T], explicit2: TYPECLASS2[T]): Matcher[T] = matcher[T](explicit1, explicit2)

/*
  // (equal (7) and ...)
  def and[U <: SUPERCLASS](rightMatcher: Matcher[U]): MatcherGen1[U, TYPECLASS] =
    new MatcherGen1[U, TYPECLASS] {
      def matcher[V <: U : TYPECLASS]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  // (equal (7) or ...)
  def or[U <: SUPERCLASS](rightMatcher: Matcher[U]): MatcherGen1[U, TYPECLASS] =
    new MatcherGen1[U, TYPECLASS] {
      def matcher[V <: U : TYPECLASS]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

// Need one for the same typeclass and one for a different typeclass, yes, and can overload because
// one returns a MatcherGen1 the other a MatcherGen2.
   // "hi" should (equal ("hi") or {mockClown.hasBigRedNose; equal ("ho")})
  def or[U <: SUPERCLASS](rightMatcherGen1: MatcherGen1[U, TYPECLASS]): MatcherGen1[U, TYPECLASS] =
    new MatcherGen1[U, TYPECLASS] {
      def matcher[V <: U : TYPECLASS]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            val rightMatcher = rightMatcherGen1.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  def or[U <: SUPERCLASS, TYPECLASS2](rightMatcherGen1: MatcherGen1[U, TYPECLASS2]): MatcherGen2[U, TYPECLASS, TYPECLASS2] =
    new MatcherGen2[U, TYPECLASS, TYPECLASS2] {
      def matcher[V <: U : TYPECLASS : TYPECLASS2]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            val rightMatcher = rightMatcherGen1.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  // "hi" should (equal ("ho") and {mockClown.hasBigRedNose; equal ("ho")})
  def and[U <: SUPERCLASS](rightMatcherGen1: MatcherGen1[U, TYPECLASS]): MatcherGen1[U, TYPECLASS] =
    new MatcherGen1[U, TYPECLASS] {
      def matcher[V <: U : TYPECLASS]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            val rightMatcher = rightMatcherGen1.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  def and[U <: SUPERCLASS, TYPECLASS2](rightMatcherGen1: MatcherGen1[U, TYPECLASS2]): MatcherGen1[U, TYPECLASS, TYPECLASS2] =
    new MatcherGen2[U, TYPECLASS, TYPECLASS2] {
      def matcher[V <: U : TYPECLASS : TYPECLASS2]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherGen1.matcher
            val rightMatcher = rightMatcherGen1.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }
*/
}
