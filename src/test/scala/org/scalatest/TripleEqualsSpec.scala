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

import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

class TripleEqualsSpec extends Spec with TripleEquals {

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
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[GenSeq[Int]]) // non-Seq superclass on right
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[Iterable[Int]])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[GenIterable[Int]])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[Traversable[Int]])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[GenTraversable[Int]])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[TraversableOnce[Int]])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[GenTraversableOnce[Int]])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[PartialFunction[Int, Int]])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[(Int) ⇒ Int])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[AnyRef])
      assert(Vector(1, 2, 3) === IndexedSeq(1, 2, 3).asInstanceOf[Any])
      assert(IndexedSeq(1, 2, 3).asInstanceOf[GenSeq[Int]] === Vector(1, 2, 3)) // non-Seq superclass on left
      assert(IndexedSeq(1, 2, 3).asInstanceOf[Iterable[Int]] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[GenIterable[Int]] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[Traversable[Int]] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[GenTraversable[Int]] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[TraversableOnce[Int]] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[GenTraversableOnce[Int]] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[PartialFunction[Int, Int]] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[(Int) ⇒ Int] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[AnyRef] === Vector(1, 2, 3))
      assert(IndexedSeq(1, 2, 3).asInstanceOf[Any] === Vector(1, 2, 3)) // non-Seq superclass on left
    }

    def `should, for Seq's type parameter, compare subclasses with superclases on either side as well as the same type on either side` {
      class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
      trait Crunchy
      class Apple extends Fruit with Crunchy
      class GrannySmith extends Apple
      class Orange extends Fruit
 
      val fruits: List[Fruit] = List(new GrannySmith, new GrannySmith)
      val apples: Vector[Apple] = Vector(new GrannySmith, new GrannySmith)
      val grannySmiths: Seq[GrannySmith] = Vector(new GrannySmith, new GrannySmith)
      val crunchies: LinearSeq[Crunchy] = List(new GrannySmith, new GrannySmith)
      val oranges: IndexedSeq[Orange] = IndexedSeq(new Orange, new Orange)

/*
      class Anyifier(o: Any) {
        def asAny: Any = o
      }

      implicit def convertToAnyifier(o: Any) = new Anyifier(o)
      assert(fruits === crunchies.asAny) // This does get it to work
      // I like this better. More guessable, because like asInstanceOf[Any]

      assert(fruits === any(crunchies)) // This does get it to work
   
*/

      assert(fruits === fruits)
      assert(apples === fruits)
      assert(fruits === apples)
      assert(fruits === grannySmiths)
      assert(grannySmiths === fruits)

      // Equality and StrictEquality
      // TripleEquals and StrictTripleEquals
     /*
         Yes, I think the general one is plain old TripleEquals, which works on Any Any. StrictTripleEquals
         overrides that, and does the typesafe thing. And there if you have a pain whereby one won't work, you
         can call .asAny. So main thing would be to get Seq, Set, and Map symetrical if possible.

         Yes, TripleEquals is just for assertions. You get them already for matchers. All StrictTripleEquals
         does is override the ah, the typeclass in regular TripleEquals. What I want to do is mix in or import
         StrictTripleEquals and LenientTripleEquals anywhere.

         Ah, can just say TripleEquals and StrictTripleEquals. And Matchers will extend TripleEquals, so if
         you mix in Matcher, you can also say assert(a === b). So you can say Matchers with StrictTripleEquals.
         There is a possibility I could just support plain old TripleEquals in Assertions, except then I have
         the problem of what if they have another === operator they want to use. 

         XMLEquality can just offer a type class for XML Nodes. Maybe it must be mixed into something that is
         at least a TripleEquals. Or maybe not. Maybe it is just a type class.

         And I'm thinking the type class should be triggered by the right hand side only perhaps? And what you
         can compare is Any, T

  abstract class Equality[T] {
    def areEqual(a: Any, b: T): Boolean
  }

        Because usually it is actual on the left, expected on the right
        assert(actual === expected)
        actual should === (expected)

        should gets put on Any, === on Any, then for expected we need a type class for the expected type.
        No, I think that screws up my implicit conversion. Needs to be A => B.
     */

      // assert(fruits === crunchies) // This does not compile as there is no direct subtype supertype relastionship
      // assert(crunchies === fruits) // An example where this === is too restrictive

      assert(apples === apples)
      assert(apples === grannySmiths)
      assert(grannySmiths === apples)
      assert(apples === crunchies)
      assert(crunchies === apples)

      assert(grannySmiths === grannySmiths)
      assert(grannySmiths === crunchies)
      assert(crunchies === grannySmiths)

      // Should not compile
      // assert(apples === oranges)
    }

    def `should compare two Sets for equality` {

      assert(HashSet(1, 2, 3) === Set(1, 2, 3)) // superclass on right
      assert(Set(1, 2, 3) === HashSet(1, 2, 3)) // superclass on left
      assert(BitSet(1, 2, 3) === HashSet(1, 2, 3)) // no inheritance relationship
      assert(HashSet(1, 2, 3) === BitSet(1, 2, 3)) // converse of above
      assert(Set(1, 2, 3) === Set(1, 2, 3)) // same type on both sides
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[GenSet[Int]]) // non-Set superclass on right
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[Iterable[Int]])
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[GenIterable[Int]])
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[Traversable[Int]])
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[GenTraversable[Int]])
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[TraversableOnce[Int]])
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[GenTraversableOnce[Int]])
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[(Int) ⇒ Boolean])
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[AnyRef])
      assert(HashSet(1, 2, 3) === Set(1, 2, 3).asInstanceOf[Any])
      assert(Set(1, 2, 3).asInstanceOf[GenSet[Int]] === HashSet(1, 2, 3)) // non-Set superclass on left
      assert(Set(1, 2, 3).asInstanceOf[Iterable[Int]] === HashSet(1, 2, 3))
      assert(Set(1, 2, 3).asInstanceOf[GenIterable[Int]] === HashSet(1, 2, 3))
      assert(Set(1, 2, 3).asInstanceOf[Traversable[Int]] === HashSet(1, 2, 3))
      assert(Set(1, 2, 3).asInstanceOf[GenTraversable[Int]] === HashSet(1, 2, 3))
      assert(Set(1, 2, 3).asInstanceOf[TraversableOnce[Int]] === HashSet(1, 2, 3))
      assert(Set(1, 2, 3).asInstanceOf[GenTraversableOnce[Int]] === HashSet(1, 2, 3))
      assert(Set(1, 2, 3).asInstanceOf[(Int) ⇒ Boolean] === HashSet(1, 2, 3))
      assert(Set(1, 2, 3).asInstanceOf[AnyRef] === HashSet(1, 2, 3))
      assert(Set(1, 2, 3).asInstanceOf[Any] === HashSet(1, 2, 3))
    }

    def `should compare two Maps for equality` {

      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3)) // supertype on right
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3) === HashMap(1 -> 1, 2 -> 2, 3 -> 3)) // supertype on left
      assert(TreeMap(1 -> 1, 2 -> 2, 3 -> 3) === HashMap(1 -> 1, 2 -> 2, 3 -> 3)) // no inheritance relationship
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === TreeMap(1 -> 1, 2 -> 2, 3 -> 3)) // converse of above
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3)) // same type on both sides
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[PartialFunction[Int, Int]]) // non-Map supertype on right
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[(Int) ⇒ Int])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[GenMap[Int, Int]])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[Iterable[(Int, Int)]])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[GenIterable[(Int, Int)]])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[Traversable[(Int, Int)]])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[GenTraversable[(Int, Int)]])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[TraversableOnce[(Int, Int)]])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[GenTraversableOnce[(Int, Int)]])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[AnyRef])
      assert(HashMap(1 -> 1, 2 -> 2, 3 -> 3) === Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[Any])
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[PartialFunction[Int, Int]] === HashMap(1 -> 1, 2 -> 2, 3 -> 3)) // non-Map supertype on left
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[(Int) ⇒ Int] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[GenMap[Int, Int]] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[Iterable[(Int, Int)]] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[GenIterable[(Int, Int)]] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[Traversable[(Int, Int)]] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[GenTraversable[(Int, Int)]] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[TraversableOnce[(Int, Int)]] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[GenTraversableOnce[(Int, Int)]] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[AnyRef] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
      assert(Map(1 -> 1, 2 -> 2, 3 -> 3).asInstanceOf[Any] === HashMap(1 -> 1, 2 -> 2, 3 -> 3))
    }

    def `should, for Map's value type parameter, compare subclasses with superclases on either side as well as the same type on either side` {
      class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
      trait Crunchy
      class Apple extends Fruit with Crunchy
      class GrannySmith extends Apple
      class Orange extends Fruit
 
      val fruits: Map[Int, Fruit] = Map(1 -> new GrannySmith, 2 -> new GrannySmith)
      val apples: Map[Int, Apple] = Map(1 -> new GrannySmith, 2 -> new GrannySmith)
      val grannySmiths: Map[Int, GrannySmith] = Map(1 -> new GrannySmith, 2 -> new GrannySmith)
      val crunchies: Map[Int, Crunchy] = Map(1 -> new GrannySmith, 2 -> new GrannySmith)
      val oranges: Map[Int, Orange] = Map(1 -> new Orange, 2 -> new Orange)

      assert(fruits === fruits)
      assert(apples === fruits)
      assert(fruits === apples)
      assert(fruits === grannySmiths)
      assert(grannySmiths === fruits)
      assert(fruits === crunchies) // This does not compile as there is no direct subtype supertype relastionship
      assert(crunchies === fruits) // An example where this === is too restrictive

      assert(apples === apples)
      assert(apples === grannySmiths)
      assert(grannySmiths === apples)
      assert(apples === crunchies)
      assert(crunchies === apples)

      assert(grannySmiths === grannySmiths)
      assert(grannySmiths === crunchies)
      assert(crunchies === grannySmiths)

      assert(apples === oranges)
    }

    def `should, for Map's key type parameter, compare only the exact same type on either side` {

      class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
      trait Crunchy
      class Apple extends Fruit with Crunchy
      class GrannySmith extends Apple
      class Orange extends Fruit
 
      val fruits: Map[Fruit, Int] = Map(new GrannySmith -> 1, new GrannySmith -> 2)
      val apples: Map[Apple, Int] = Map(new GrannySmith -> 1, new GrannySmith -> 2)
      val grannySmiths: Map[GrannySmith, Int] = Map(new GrannySmith -> 1, new GrannySmith -> 2)
      val crunchies: Map[Crunchy, Int] = Map(new GrannySmith -> 1, new GrannySmith -> 2)
      val oranges: Map[Orange, Int] = Map(new Orange -> 1, new Orange -> 2)

      assert(fruits === fruits)
      assert(apples === fruits)
      assert(fruits === apples)
      assert(fruits === grannySmiths)
      assert(grannySmiths === fruits)
      assert(fruits === crunchies) // This does not compile as there is no direct subtype supertype relastionship
      assert(crunchies === fruits) // An example where this === is too restrictive

      assert(apples === apples)
      assert(apples === grannySmiths)
      assert(grannySmiths === apples)
      assert(apples === crunchies)
      assert(crunchies === apples)

      assert(grannySmiths === grannySmiths)
      assert(grannySmiths === crunchies)
      assert(crunchies === grannySmiths)

      assert(apples === oranges)
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

      // Starting from Char
      assert('A' === 65) // Char => Int
      assert(65 === 'A')

      assert('A' === 65L) // Char => Long
      assert(65L === 'A')

      assert('A' === 65.0F) // Char => Float
      assert(65.0F === 'A')

      assert('A' === 65.0) // Char => Double
      assert(65.0 === 'A')

      // Starting from Byte
      assert(1.toByte === 1.toShort) // Byte => Short
      assert(1.toShort === 1.toByte)

      assert(1.toByte === 1) // Byte => Int
      assert(1 === 1.toByte)

      assert(1.toByte === 1L) // Byte => Long
      assert(1L === 1.toByte)

      assert(1.toByte === 1.0F) // Byte => Float
      assert(1.0F === 1.toByte)

      assert(1.toByte === 1.0) // Byte => Double
      assert(1.0 === 1.toByte)

      // Starting from Short
      assert(1.toShort === 1) // Short => Int
      assert(1 === 1.toShort)

      assert(1.toShort === 1L) // Short => Long
      assert(1L === 1.toShort)

      assert(1.toShort === 1.0F) // Short => Float
      assert(1.0F === 1.toShort)

      assert(1.toShort === 1.0) // Short => Double
      assert(1.0 === 1.toShort)

      // Starting from Int
      assert(1 === 1L) // Int => Long
      assert(1L === 1)

      assert(1 === 1.0F) // Int => Float
      assert(1.0F === 1)

      assert(1 === 1.0) // Int => Double
      assert(1.0 === 1)

      // Starting from Long
      assert(1L === 1.0F) // Long => Float
      assert(1.0F === 1L)

      assert(1L === 1.0) // Long => Double
      assert(1.0 === 1L)

      // Starting from Float
      assert(1.0F === 1.0) // Float => Double
      assert(1.0 === 1.0F)
    }

    def `should work with arbritrary types` {
      intercept[TestFailedException] {
        assert("1" === 1)
      }
      intercept[TestFailedException] {
        assert(List(1, 2, 3) === Map(1 -> 1, 2 -> 2, 3 -> 3))
      }
      intercept[TestFailedException] {
        assert(Map(1 -> 1, 2 -> 2, 3 -> 3) === List(1, 2, 3))
      }
      intercept[TestFailedException] {
        assert(Set(1, 2, 3) === List(1, 2, 3))
      }
      intercept[TestFailedException] {
        assert(List(1, 2, 3) === Set(1, 2, 3))
      }
    }
  }
}
  
