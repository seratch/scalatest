/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest.fixture

import org.scalatest._

/**
 * A sister trait to <code>org.scalatest.FreeSpec</code> that can pass multiple types of fixture objects into its tests.
 *
 * <p>
 * This trait behaves similarly to trait <code>org.scalatest.FreeSpec</code>, except that tests may take a fixture object, and unlike
 * a <code>FixtureFreeSpec</code>, different tests may take different types of fixtures. This trait extends <code>FixtureFreeSpec</code>
 * and mixes in <code>ConfigMapFixture</code>, which defines the <code>Fixture</code> type to be the <code>configMap</code>'s
 * type (<code>Map[String, Any]</code>) and defines the <code>withFixture</code> method to simply pass the <code>configMap</code>
 * to the test function. To write tests that take fixtures of types other than <code>Fixture</code> (<em>i.e.</em>,
 * <code>Map[String, Any]</code>), you can define implicit conversions from a function of type <code>(</code>&lt;the fixture type&gt;<code>) =&gt; Unit</code>
 * to a function of type <code>(FixtureParam) =&gt; Unit</code>. Each implicit conversion method serves as the with-fixture method for that type.
 * </p>
 * 
 * <p>
 * Subclasses of this trait must, therefore, do two things differently from a plain old <code>org.scalatest.FreeSpec</code>:
 * </p>
 * 
 * <ol>
 * <li>define implicit <code>with<em>&lt;type&gt;</em>Fixture</code> methods</li>
 * <li>write tests that take the different fixture types for which you've defined implicit conversion methods (You can also define tests that don't take a <code>Fixture</code>.)</li>
 * </ol>
 *
 * <p>
 * Here's an example that has two fixture types, <code>String</code> and <code>List[Int]</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.fixture.MultipleFixtureFreeSpec
 * 
 * class MyFreeSpec extends MultipleFixtureFreeSpec {
 * 
 *   // A test that takes a String fixture
 *   implicit def withStringFixture(testFun: String => Unit): FixtureParam => Unit =
 *     configMap => testFun("howdy")
 * 
 *   // The "with-fixture" method for tests that take a List[Int] fixture
 *   implicit def withListFixture(testFun: List[Int] => Unit): FixtureParam => Unit =
 *     configMap => testFun(List(configMap.size))
 * 
 *   "Tests in a MultipleFixtureFreeSpec" - {
 *
 *     // A test that takes a String fixture
 *     "should take a string fixture" in { (s: String) =>
 *       assert(s === "howdy")
 *     }
 * 
 *     // A test that takes a List[Int] fixture
 *     "should take a list fixture" in { (list: List[Int]) =>
 *       assert(list.size === 1)
 *     }
 * 
 *     // A test that takes no fixture
 *     "should take no fixture" in { () =>
 *       assert(1 === 1)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.fixture.MultipleFixtureFreeSpec
 * <br /><span class="stReserved">class</span> <span class="stType">MyFreeSpec</span> <span class="stReserved">extends</span> <span class="stType">MultipleFixtureFreeSpec</span> {
 * <br />  <span class="stLineComment">// A test that takes a String fixture</span>
 *   <span class="stReserved">implicit</span> <span class="stReserved">def</span> withStringFixture(testFun: <span class="stType">String</span> => <span class="stType">Unit</span>): <span class="stType">FixtureParam</span> => <span class="stType">Unit</span> =
 *     configMap => testFun(<span class="stQuotedString">"howdy"</span>)
 * <br />  <span class="stLineComment">// The "with-fixture" method for tests that take a List[Int] fixture</span>
 *   <span class="stReserved">implicit</span> <span class="stReserved">def</span> withListFixture(testFun: <span class="stType">List[Int]</span> => <span class="stType">Unit</span>): <span class="stType">FixtureParam</span> => <span class="stType">Unit</span> =
 *     configMap => testFun(<span class="stType">List</span>(configMap.size))
 * <br />  <span class="stQuotedString">"Tests in a MultipleFixtureFreeSpec"</span> - {
 * <br />    <span class="stLineComment">// A test that takes a String fixture</span>
 *     <span class="stQuotedString">"should take a string fixture"</span> in { (s: <span class="stType">String</span>) =>
 *       assert(s === <span class="stQuotedString">"howdy"</span>)
 *     }
 * <br />    <span class="stLineComment">// A test that takes a List[Int] fixture</span>
 *     <span class="stQuotedString">"should take a list fixture"</span> in { (list: <span class="stType">List[Int]</span>) =>
 *       assert(list.size === <span class="stLiteral">1</span>)
 *     }
 * <br />    <span class="stLineComment">// A test that takes no fixture</span>
 *     <span class="stQuotedString">"should take no fixture"</span> in { () =>
 *       assert(<span class="stLiteral">1</span> === <span class="stLiteral">1</span>)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * The first method in this class, <code>withStringFixture</code>, is the implicit conversion function for tests that take a fixture
 * of type <code>String</code>.  In this contrived example, the hard-coded string <code>"howdy"</code> is passed into the test:
 * </p>
 *
 * <pre class="stHighlight">
 * implicit def withStringFixture(testFun: String => Unit): FixtureParam => Unit =
 *   configMap => testFun("howdy")
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">implicit</span> <span class="stReserved">def</span> withStringFixture(testFun: <span class="stType">String</span> => <span class="stType">Unit</span>): <span class="stType">FixtureParam</span> => <span class="stType">Unit</span> =
 *   configMap => testFun(<span class="stQuotedString">"howdy"</span>)
 * </pre>
 * 
 * <p>
 * Although the string fixture doesn't need anything from the config map, you still need the <code>configMap =></code> at
 * the beginning of the result function to get the expression to type check.
 * </p>
 *
 * <p>
 * The next method in this class, <code>withListFixture</code>, is the implicit conversion function for tests that take a
 * fixture of type <code>List[Int]</code>.  In this contrived example, a <code>List[Int]</code> that contains one element, the
 * size of the <code>configMap</code>, is passed to the test function. 
 * </p>
 *
 * <p>
 * Following the implicit conversion methods are the test declarations. One test is written to take the <code>String</code> fixture:
 * </p>
 *
 * <pre class="stHighlight">
 * "take a string fixture" in { (s: String) =>
 *   assert(s === "howdy")
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stQuotedString">"take a string fixture"</span> in { (s: <span class="stType">String</span>) =>
 *   assert(s === <span class="stQuotedString">"howdy"</span>)
 * }
 * </pre>
 *
 * <p>
 * What happens at compile time is that because the <code>Fixture</code> type is <code>Map[String, Any]</code>, the <code>test</code> method
 * should be passed a function from type <code>(Map[String, Any]) => Unit</code>, or using the type alias, <code>(FixtureParam) => Unit</code>. Passing
 * a function of type <code>String => Unit</code> as is attempted here is a type error. Thus the compiler will look around for an implicit
 * conversion that will fix the type error, and will find the <code>withStringFixture</code> method. Because this is the only implicit
 * conversion that fixes the type error, it will apply it, effectively generating this code:
 * </p>
 *
 * <pre class="stHighlight">
 * // after the implicit withStringFixture method is applied by the compiler
 * "should take a string fixture" in {
 *   withStringFixture { (s: String) =>
 *     assert(s === "howdy")
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stLineComment">// after the implicit withStringFixture method is applied by the compiler</span>
 * <span class="stQuotedString">"should take a string fixture"</span> in {
 *   withStringFixture { (s: <span class="stType">String</span>) =>
 *     assert(s === <span class="stQuotedString">"howdy"</span>)
 *   }
 * }
 * </pre>
 *
 * <p>
 * After passing the <code>(String) => Unit</code> function to <code>withStringFixture</code>, the result will be of
 * type <code>(FixtureParam) => Unit</code>, which the <code>test</code> method expects.
 * </p>
 *
 * <p>
 * The next test is written to take the <code>List[Int]</code> fixture:
 * </p>
 *
 * <pre class="stHighlight">
 * "should take a list fixture" in { (list: List[Int]) =>
 *   assert(list.size === 1)
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stQuotedString">"should take a list fixture"</span> in { (list: <span class="stType">List[Int]</span>) =>
 *   assert(list.size === <span class="stLiteral">1</span>)
 * }
 * </pre>
 *
 * <p>
 * The compiler will apply the <code>withListFixture</code> implicit conversion in this case, effectively generating the following
 * code:
 * </p>
 *
 * <pre class="stHighlight">
 * "should take a list fixture" in {
 *   withListFixture { (list: List[Int]) =>
 *     assert(list.size === 1)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stQuotedString">"should take a list fixture"</span> in {
 *   withListFixture { (list: <span class="stType">List[Int]</span>) =>
 *     assert(list.size === <span class="stLiteral">1</span>)
 *   }
 * }
 * </pre>
 * 
 * <p>
 * Note that in a <code>FixtureFreeSpec</code>, you need to specify the type of the fixture explicitly so the compiler knows
 * the type to convert from. So you must, for example, write:
 * </p>
 *
 * <pre class="stHighlight">
 * "should take a list fixture" in { (list: List[Int]) =>
 *   assert(list.size === 1)
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stQuotedString">"should take a list fixture"</span> in { (list: <span class="stType">List[Int]</span>) =>
 *   assert(list.size === <span class="stLiteral">1</span>)
 * }
 * </pre>
 *
 * <p>
 * The following attempt will fail to compile:
 * </p>
 *
 * <pre class="stHighlight">
 * // won't compile, because list is inferred to be of type FixtureParam
 * "should take a list fixture" in { list =>
 *   assert(list.size === 1)
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stLineComment">// won't compile, because list is inferred to be of type FixtureParam</span>
 * <span class="stQuotedString">"should take a list fixture"</span> in { list =>
 *   assert(list.size === <span class="stLiteral">1</span>)
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait MultipleFixtureFreeSpec extends FixtureFreeSpec with ConfigMapFixture