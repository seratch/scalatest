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

trait CustomEquality extends NonImplicitAssertions with LowPriorityEqualityImplicits {

  abstract class EqualityResult[A, B]
  case class UnexpectedEqualityResult[A, B](msg: String) extends EqualityResult[A, B]
  case class ExpectedEqualityResult[A, B] extends EqualityResult[A, B]
  
  class Equalizer[L](left: L) {
    def ===[R](right: R)(implicit equality: Equality[L, R]): EqualityResult[L, R] = {
      if (equality.areEqual(left, right)) ExpectedEqualityResult()
      else UnexpectedEqualityResult(left + " did not equal " + right)
    }
    def !==[R](right: R)(implicit equality: Equality[L, R]): EqualityResult[L, R] = {
      if (!equality.areEqual(left, right)) ExpectedEqualityResult()
      else UnexpectedEqualityResult(left + " did not equal " + right)
    }
  }
  
  class SeqEqualizer[L](left: Seq[L]) {
    println("Created a SeqEqualizer")
    def ===[R](right: Seq[R])(implicit equality: Equality[Seq[L], Seq[R]]): EqualityResult[Seq[L], Seq[R]] = {
      if (equality.areEqual(left, right)) ExpectedEqualityResult()
      else UnexpectedEqualityResult(left + " did not equal " + right)
    }
  }
  
  class MapEqualizer[LK, LV](left: Map[LK, LV]) {
    println("Created a MapEqualizer")
    def ===[RK, RV](right: Map[RK, RV])(implicit equality: Equality[Map[LK, LV], Map[RK, RV]]): EqualityResult[Map[LK, LV], Map[RK, RV]] = {
      if (equality.areEqual(left, right)) ExpectedEqualityResult()
      else UnexpectedEqualityResult(left + " did not equal " + right)
    }
  }
  
  class SetEqualizer[L](left: Set[L]) {
    println("Created a SetEqualizer")
    def ===[R](right: Set[R])(implicit equality: Equality[Set[L], Set[R]]): EqualityResult[Set[L], Set[R]] = {
      if (equality.areEqual(left, right)) ExpectedEqualityResult()
      else UnexpectedEqualityResult(left + " did not equal " + right)
    }
  }
  
  def assert[L, R](eqRes: EqualityResult[L, R]) {
    eqRes match {
      case UnexpectedEqualityResult(msg) => throw new AssertionError(msg)
      case _ =>
    }
  }

  implicit def convertToEqualizer[T](o: T) = new Equalizer(o)
  implicit def convertToSeqEqualizer[T](o: Seq[T]) = new SeqEqualizer[T](o)
  implicit def convertToMapEqualizer[K, V](o: Map[K, V]) = new MapEqualizer[K, V](o)
  implicit def convertToSetEqualizer[T](o: Set[T]) = new SetEqualizer[T](o)
  
  implicit def equalityTwo[A, B](implicit conv: B => A): Equality[A, B] =
    new Equality[A, B] {
      def areEqual(a: A, b: B): Boolean =
        a == conv(b)
    }

/*
  implicit def equalityTwo[A, B](implicit ev: B <:< A): Equality[A, B] =
    new Equality[A, B] {
      def areEqual(a: A, b: B): Boolean =
        a == b
    }
*/
}

object CustomEquality extends CustomEquality

