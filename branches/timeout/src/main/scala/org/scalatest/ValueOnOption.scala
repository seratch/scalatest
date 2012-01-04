/*
 * Copyright 2001-2011 Artima, Inc.
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

import java.util.NoSuchElementException

// TODO: Make this private[scalatest]
trait ValueOnOption {

  implicit def convertToValuable[T](opt: Option[T]) = new Valuable(opt)

  class Valuable[T](opt: Option[T]) {
    def value: T = {
      try {
        opt.get
      }
      catch {
        case cause: NoSuchElementException => // TODO: Grab the string from the resource file
          // TODO: Verify and possibly be smarter about stack depth
          throw new TestFailedException("The Option on which value was invoked was not defined.", cause, 1)
      }
    }
  }
}

object ValueOnOption extends ValueOnOption