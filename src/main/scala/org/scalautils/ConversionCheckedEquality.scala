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
package org.scalautils

trait LowPriorityConversionCheckedEquality extends EqualityConstraints {
  implicit override def lowPriorityConversionCheckedEqualityConstraint[A, B](implicit cnv: A => B): EqualityConstraint[A, B] = new EqualityConstraint[A, B]
}

trait ConversionCheckedEquality extends LowPriorityConversionCheckedEquality {

  override def unconstrainedEquality[A, B]: EqualityConstraint[A, B] = new EqualityConstraint[A, B]

  override def lowPriorityTypeCheckedEqualityConstraint[A, B](implicit ev: A <:< B): EqualityConstraint[A, B] = new EqualityConstraint[A, B]
  override def typeCheckedEqualityConstraint[A, B](implicit ev: B <:< A): EqualityConstraint[A, B] = new EqualityConstraint[A, B]

  implicit override def conversionCheckedEqualityConstraint[A, B](implicit cnv: B => A): EqualityConstraint[A, B] = new BToAEqualityConstraint[A, B](cnv)
}

object ConversionCheckedEquality extends ConversionCheckedEquality

