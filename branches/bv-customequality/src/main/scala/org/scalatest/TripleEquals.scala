/*
 * Copyright 2001-20012 Artima, Inc.
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
package org.scalatest

trait TripleEquals extends Assertions {

  /**
   * Overrides the <code>super</code> implementation of <code>convertToEqualizer</code>, turning off the implicit 
   * modifier (if present) to remove the method from the space of implicit conversions.
   *
   * @param left the object whose type to convert to <code>Equalizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  override def convertToEqualizer(right: Any): Equalizer = super.convertToEqualizer(right)

  abstract class EqualityResult[A, B]
  case class UnexpectedEqualityResult[A, B](msg: String) extends EqualityResult[A, B]
  case class ExpectedEqualityResult[A, B] extends EqualityResult[A, B]

  class AnyEqualizer[L](left: L) {
    def ===[R](right: R)(implicit equality: Equality[L, R]): EqualityResult[L, R] = {
      if (equality.areEqual(left, right)) ExpectedEqualityResult()
      else UnexpectedEqualityResult(left + " did not equal " + right)
    }
    def !==[R](right: R)(implicit equality: Equality[L, R]): EqualityResult[L, R] = {
      if (!equality.areEqual(left, right)) ExpectedEqualityResult()
      else UnexpectedEqualityResult(left + " equaled " + right)
    }
  }
  
  def assert[L, R](eqRes: EqualityResult[L, R]) {
    eqRes match {
      case UnexpectedEqualityResult(msg) => throw new TestFailedException(msg, 0)
      case _ =>
    }
  }

  implicit def convertToAnyEqualizer[T](o: T) = new AnyEqualizer(o)

  implicit def anyEquality[A, B]: Equality[A, B] =
    new Equality[A, B] {
      def areEqual(a: A, b: B): Boolean =
        a == b
    }
}

object TripleEquals extends TripleEquals

