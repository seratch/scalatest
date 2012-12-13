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
package org.scalautils

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

class CopiedForLegacyTripleEqualsSpec extends Spec with NonImplicitAssertions {

  case class Super(size: Int)
  class Sub(sz: Int) extends Super(sz)

  val super1: Super = new Super(1)
  val sub1: Sub = new Sub(1)
  val super2: Super = new Super(2)
  val sub2: Sub = new Sub(2)
  val nullSuper: Super = null

  object `the custom equality === operator` {

    object `with LegacyTripleEquals` {

      def `should compare anything with anything` {

        new LegacyTripleEquals {

          assert(1 === 1)
          assert((1 !== 1).isDefined)

          assert(1 === 1L)
          assert((1 !== 1L).isDefined)

          assert(1L === 1)
          assert((1L !== 1).isDefined)

          assert("1" !== 1)
          assert(("1" === 1).isDefined)

          assert(1 !== "1")
          assert((1 === "1").isDefined)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          assert(sub2 !== super1)
          assert(super1 === super1)
          assert(super1 === sub1)
          assert(sub1 === super1)

          assert((super1 === null).isDefined)
          assert(super1 !== null)

          assert(nullSuper === null)
          assert((nullSuper !== null).isDefined)
          assert((nullSuper === super1).isDefined)
          assert(nullSuper !== super1)
        }
      }

/*
      def `should be overridable with TypeCheckedLegacyTripleEquals locally when LegacyTripleEquals imported` {

        object O extends LegacyTripleEquals
        import O._

        new TypeCheckedLegacyTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          assert(1 === 1)
          assert((1 !== 1).isDefined)

          assert(ap === fr)
          assert(fr === ap)
          assert(ap === cr)
          assert(cr === ap)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          assert(sub2 !== super1)
          assert(super1 === super1)
          assert(super1 === sub1)
          assert(sub1 === super1)

          // The rest should not compile
          // assert(1 === 1L)
          // assert(1L === 1)
          // assert(1 !== 1L)
          // assert(1L !== 1)

          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
        }
      }

      def `should be overridable with TypeCheckedLegacyTripleEquals locally when LegacyTripleEquals mixed in` {

        object O extends LegacyTripleEquals {

          new TypeCheckedLegacyTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy
  
            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple
  
            assert(1 === 1)
            assert((1 !== 1).isDefined)
  
            assert(ap === fr)
            assert(fr === ap)
            assert(ap === cr)
            assert(cr === ap)

            assert(super1 !== super2)
            assert(super1 !== sub2)
            assert(sub2 !== super1)
            assert(super1 === super1)
            assert(super1 === sub1)
            assert(sub1 === super1)
  
            // The rest should not compile
            // assert(1 === 1L)
            // assert(1L === 1)
            // assert(1 !== 1L)
            // assert(1L !== 1)
  
            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")
  
            // assert(fr === cr)
            // assert(cr === fr)
          }
        }
      }

      def `should be overridable with ConversionCheckedLegacyTripleEquals locally when LegacyTripleEquals imported` {

        object O extends LegacyTripleEquals
        import O._

        new ConversionCheckedLegacyTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            assert(1 === 1)
            assert((1 !== 1).isDefined)

            assert(ap === fr)
            assert(fr === ap)
            assert(ap === cr)
            assert(cr === ap)

            assert(super1 !== super2)
            assert(super1 !== sub2)
            assert(sub2 !== super1)
            assert(super1 === super1)
            assert(super1 === sub1)
            assert(sub1 === super1)

            // These should work with implicit conversions
            assert(1 === 1L)
            assert(1L === 1)
            assert((1 !== 1L).isDefined)
            assert((1L !== 1).isDefined)

            // The rest should not compile
            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")

            // assert(fr === cr)
            // assert(cr === fr)
        }
      }

      def `should be overridable with ConversionCheckedLegacyTripleEquals locally when LegacyTripleEquals mixed in` {

        object O extends LegacyTripleEquals {

          new ConversionCheckedLegacyTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            assert(1 === 1)
            assert((1 !== 1).isDefined)

            assert(ap === fr)
            assert(fr === ap)
            assert(ap === cr)
            assert(cr === ap)

            assert(super1 !== super2)
            assert(super1 !== sub2)
            assert(sub2 !== super1)
            assert(super1 === super1)
            assert(super1 === sub1)
            assert(sub1 === super1)

            // These should work with implicit conversions
            assert(1 === 1L)
            assert(1L === 1)
            assert((1 !== 1L).isDefined)
            assert((1L !== 1).isDefined)

            // The rest should not compile
            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")

            // assert(fr === cr)
            // assert(cr === fr)
          }
        }
      }
*/
    }

    object `with TypeCheckedLegacyTripleEquals` {

      def `should compare supertypes with subtypes on either side` {

        new TypeCheckedLegacyTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          assert(1 === 1)
          assert((1 !== 1).isDefined)

          assert(ap === fr)
          assert(fr === ap)
          assert(ap === cr)
          assert(cr === ap)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          assert(sub2 !== super1)
          assert(super1 === super1)
          assert(super1 === sub1)
          assert(sub1 === super1)

          assert((super1 === null).isDefined)
          assert(super1 !== null)

          assert(nullSuper === null)
          assert((nullSuper !== null).isDefined)
          assert((nullSuper === super1).isDefined)
          assert(nullSuper !== super1)

          // The rest should not compile
          // assert(1 === 1L)
          // assert(1L === 1)
          // assert(1 !== 1L)
          // assert(1L !== 1)

          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
        }
      }

/*
      def `should be overridable with LegacyTripleEquals locally when TypeCheckedLegacyTripleEquals imported` {

        object O extends TypeCheckedLegacyTripleEquals
        import O._

        new LegacyTripleEquals {

          assert(1 === 1)
          assert((1 !== 1).isDefined)

          assert(1 === 1L)
          assert((1 !== 1L).isDefined)

          assert(1L === 1)
          assert((1L !== 1).isDefined)

          assert("1" !== 1)
          assert(("1" === 1).isDefined)

          assert(1 !== "1")
          assert((1 === "1").isDefined)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
          assert(super1 === super1)
          assert(super1 === sub1)
          // assert(sub1 === super1) // compiles on 2.10 but not 2.9
        }
      }

      def `should be overridable with LegacyTripleEquals locally when TypeCheckedLegacyTripleEquals mixed in` {

        object O extends TypeCheckedLegacyTripleEquals {

          new LegacyTripleEquals {

            assert(1 === 1)
            assert((1 !== 1).isDefined)

            assert(1 === 1L)
            assert((1 !== 1L).isDefined)

            assert(1L === 1)
            assert((1L !== 1).isDefined)

            assert("1" !== 1)
            assert(("1" === 1).isDefined)

            assert(1 !== "1")
            assert((1 === "1").isDefined)

            assert(super1 !== super2)
            assert(super1 !== sub2)
            // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
            assert(super1 === super1)
            assert(super1 === sub1)
            // assert(sub1 === super1) // compiles on 2.10 but not 2.9
          }
        }
      }

      def `should be overridable with ConversionCheckedLegacyTripleEquals locally when TypeCheckedLegacyTripleEquals imported` {

        object O extends TypeCheckedLegacyTripleEquals
        import O._

        new ConversionCheckedLegacyTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          // assert(1 === 1) // compiles on 2.10 but not 2.9
          // assert((1 !== 1).isDefined) // compiles on 2.10 but not 2.9

          // assert(ap === fr) // compiles on 2.10 but not 2.9
          // compiles on 2.10 but not 2.9/ assert(fr === ap) // compiles on 2.10 but not 2.9
          // assert(ap === cr) // compiles on 2.10 but not 2.9
          // assert(cr === ap) // compiles on 2.10 but not 2.9

          // assert(super1 !== super2) // compiles on 2.10 but not 2.9
          // assert(super1 !== sub2) // compiles on 2.10 but not 2.9
          // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
          // assert(super1 === super1) // compiles on 2.10 but not 2.9
          // assert(super1 === sub1) // compiles on 2.10 but not 2.9
          // assert(sub1 === super1) // compiles on 2.10 but not 2.9

          // These should work with implicit conversions
          assert(1 === 1L)
          assert(1L === 1)
          assert((1 !== 1L).isDefined)
          assert((1L !== 1).isDefined)

          // The rest should not compile
          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
        }
      }

      def `should be overridable with ConversionCheckedLegacyTripleEquals locally when TypeCheckedLegacyTripleEquals mixed in` {

        object O extends TypeCheckedLegacyTripleEquals {

          new ConversionCheckedLegacyTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            // assert(1 === 1) // compiles on 2.10 but not 2.9
            // assert((1 !== 1).isDefined) // compiles on 2.10 but not 2.9

            // assert(ap === fr) // compiles on 2.10 but not 2.9
            // assert(fr === ap) // compiles on 2.10 but not 2.9
            // assert(ap === cr) // compiles on 2.10 but not 2.9
            // assert(cr === ap) // compiles on 2.10 but not 2.9

            // assert(super1 !== super2) // compiles on 2.10 but not 2.9
            // assert(super1 !== sub2) // compiles on 2.10 but not 2.9
            // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
            // assert(super1 === super1) // compiles on 2.10 but not 2.9
            // assert(super1 === sub1) // compiles on 2.10 but not 2.9
            // assert(sub1 === super1) // compiles on 2.10 but not 2.9

            // These should work with implicit conversions
            assert(1 === 1L)
            assert(1L === 1)
            assert((1 !== 1L).isDefined)
            assert((1L !== 1).isDefined)

            // The rest should not compile
            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")

            // assert(fr === cr)
            // assert(cr === fr)
          }
        }
      }
*/
    }

    object `with ConversionCheckedLegacyTripleEquals` {

      def `should compare supertypes with subtypes on either side as well as types with implicit conversions in either direction` {

        new ConversionCheckedLegacyTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          assert(1 === 1)
          assert((1 !== 1).isDefined)

          assert(ap === fr)
          assert(fr === ap)
          assert(ap === cr)
          assert(cr === ap)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          assert(sub2 !== super1)
          assert(super1 === super1)
          assert(super1 === sub1)
          assert(sub1 === super1)

          // These should work with implicit conversions
          assert(1 === 1L)
          assert(1L === 1)
          assert((1 !== 1L).isDefined)
          assert((1L !== 1).isDefined)

          // Should work sensibly with nulls
          assert((super1 === null).isDefined)
          assert(super1 !== null)

          assert(nullSuper === null)
          assert((nullSuper !== null).isDefined)
          assert((nullSuper === super1).isDefined)
          assert(nullSuper !== super1)

          // The rest should not compile
          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
        }
      }

/*
      def `should be overridable with LegacyTripleEquals locally when ConversionCheckedLegacyTripleEquals imported` {

        object O extends ConversionCheckedLegacyTripleEquals
        import O._

        new LegacyTripleEquals {

          assert(1 === 1)
          assert((1 !== 1).isDefined)

          // assert(1 === 1L) // compiles on 2.10 but not 2.9
          // assert((1 !== 1L).isDefined) // compiles on 2.10 but not 2.9

          assert(1L === 1)
          assert((1L !== 1).isDefined)

          assert("1" !== 1)
          assert(("1" === 1).isDefined)

          assert(1 !== "1")
          assert((1 === "1").isDefined)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
          assert(super1 === super1)
          assert(super1 === sub1)
          // assert(sub1 === super1) // compiles on 2.10 but not 2.9
        }
      }

      def `should be overridable with LegacyTripleEquals locally when ConversionCheckedLegacyTripleEquals mixed in` {

        object O extends ConversionCheckedLegacyTripleEquals {

          new LegacyTripleEquals {

            assert(1 === 1)
            assert((1 !== 1).isDefined)

            // assert(1 === 1L) // compiles on 2.10 but not 2.9
            // assert((1 !== 1L).isDefined) // compiles on 2.10 but not 2.9

            assert(1L === 1)
            assert((1L !== 1).isDefined)

            assert("1" !== 1)
            assert(("1" === 1).isDefined)

            assert(1 !== "1")
            assert((1 === "1").isDefined)

            assert(super1 !== super2)
            assert(super1 !== sub2)
            // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
            assert(super1 === super1)
            assert(super1 === sub1)
            // assert(sub1 === super1) // compiles on 2.10 but not 2.9
          }
        }
      }

      def `should be overridable with TypeCheckedLegacyTripleEquals locally when ConversionCheckedLegacyTripleEquals imported` {

        object O extends ConversionCheckedLegacyTripleEquals
        import O._

        new TypeCheckedLegacyTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          // assert(1 === 1) // compiles on 2.10 but not 2.9
          // assert((1 !== 1).isDefined) // compiles on 2.10 but not 2.9

          // assert(ap === fr) // compiles on 2.10 but not 2.9
          // assert(fr === ap) // compiles on 2.10 but not 2.9
          // assert(ap === cr) // compiles on 2.10 but not 2.9
          // assert(cr === ap) // compiles on 2.10 but not 2.9

          // assert(super1 !== super2) // compiles on 2.10 but not 2.9
          // assert(super1 !== sub2) // compiles on 2.10 but not 2.9
          // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
          // assert(super1 === super1) // compiles on 2.10 but not 2.9
          // assert(super1 === sub1) // compiles on 2.10 but not 2.9
          // assert(sub1 === super1) // compiles on 2.10 but not 2.9

          // The rest should not compile
          // assert(1 === 1L)
          // assert(1L === 1)
          // assert(1 !== 1L)
          // assert(1L !== 1)

          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
        }
      }

      def `should be overridable with TypeCheckedLegacyTripleEquals locally when ConversionCheckedLegacyTripleEquals mixed in` {

        object O extends ConversionCheckedLegacyTripleEquals {

          new TypeCheckedLegacyTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            // assert(1 === 1) // compiles on 2.10 but not 2.9
            // assert((1 !== 1).isDefined) // compiles on 2.10 but not 2.9

            // assert(ap === fr) // compiles on 2.10 but not 2.9
            // assert(fr === ap) // compiles on 2.10 but not 2.9
            // assert(ap === cr) // compiles on 2.10 but not 2.9
            // assert(cr === ap) // compiles on 2.10 but not 2.9

            // assert(super1 !== super2) // compiles on 2.10 but not 2.9
            // assert(super1 !== sub2) // compiles on 2.10 but not 2.9
            // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
            // assert(super1 === super1) // compiles on 2.10 but not 2.9
            // assert(super1 === sub1) // compiles on 2.10 but not 2.9
            // assert(sub1 === super1) // compiles on 2.10 but not 2.9

            // The rest should not compile
            // assert(1 === 1L)
            // assert(1L === 1)
            // assert(1 !== 1L)
            // assert(1L !== 1)

            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")

            // assert(fr === cr)
            // assert(cr === fr)
          }
        }
      }
*/
    }
  }
}

