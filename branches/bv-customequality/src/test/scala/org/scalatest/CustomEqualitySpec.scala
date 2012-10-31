/*
 * Copyright 2001-2008 Artima, Inc.
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

class CustomEqualitySpec extends Spec with CustomEquality {

  import scala.collection.immutable._

  object `the custom equality === operator` {

    def `should compare subclasses with superclases on either side as well as the same type on either side` {

      class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
      class Apple extends Fruit

      assert(new Fruit === new Apple)
      assert(new Apple === new Apple)
      assert(new Apple === new Fruit)

      assert(1 === 1)
    }

    def `should compare two Seqs for equality` {

      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3)) // superclass on right
      assert(IndexedSeq(1, 2, 3) === Vector(1, 2, 3)) // superclass on left
      assert(Vector(1, 2, 3) === List(1, 2, 3)) // no inheritance relationship
      assert(List(1, 2, 3) === Vector(1, 2, 3)) // converse of above
      assert(List(1, 2, 3) === List(1, 2, 3)) // same type on both sides
    }

    def `should compare two Sets for equality` {

      assert(HashSet(1, 2, 3) === Set(1, 2, 3)) // superclass on right
      assert(Set(1, 2, 3) === HashSet(1, 2, 3)) // superclass on left
      assert(BitSet(1, 2, 3) === HashSet(1, 2, 3)) // no inheritance relationship
      assert(HashSet(1, 2, 3) === BitSet(1, 2, 3)) // converse of above
      assert(Set(1, 2, 3) === Set(1, 2, 3)) // same type on both sides
    }

    def `should compare two Maps for equality` {

      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3)) // supertype on right
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3) === HashMap(1 -> 1, 2 -> 2, 3 -> 3)) // supertype on left
      assert(TreeMap(1 -> 1, 2 -> 2, 3 -> 3) === HashMap(1 -> 1, 2 -> 2, 3 -> 3)) // no inheritance relationship
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === TreeMap(1 -> 1, 2 -> 2, 3 -> 3)) // converse of above
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3)) // same type on both sides
    }

    def `should enable custom equality via a user-defined type class` {

      case class User(id: Long, name: String)

      val kat1 = User(1, "Kat")
      val joe2 = User(2, "Joe")
      val joe3 = User(3, "Joe")

      assert(kat1 === kat1)
      assert(joe2 === joe2)
      assert(joe3 === joe3)
      assert(kat1 !== joe2)
      assert(joe2 !== kat1)
      assert(joe2 !== joe3)
      assert(joe3 !== joe2)
     
      implicit val userEquality =
        new Equality[User, User] {
          def areEqual(a: User, b: User): Boolean = a.name == b.name // I.e., define equality without considering id
        }

      assert(kat1 === kat1)
      assert(joe2 === joe2)
      assert(joe3 === joe3)
      assert(kat1 !== joe2)
      assert(joe2 !== kat1)
      assert(joe2 === joe3) // These two should be different from above, because of the type class
      assert(joe3 === joe2)
    }

    def `should compile if there's an implicit conversion from one type to the other` {
      assert(1L === 1)
      assert(1 === 1L)
    }
  }
/*
  These don't compile.
  assert("1" === 1)
  assert(List(1, 2, 3) === Map(1 -> 1, 2 -> 2, 3 -> 3))
  assert(Map(1 -> 1, 2 -> 2, 3 -> 3) === List(1, 2, 3))
  assert(Set(1, 2, 3) === List(1, 2, 3))
  assert(List(1, 2, 3) === Set(1, 2, 3))
*/
}
  
