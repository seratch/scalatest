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
import FixtureNodeFamily._
import scala.collection.immutable.ListSet
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import org.scalatest.Suite.anErrorThatShouldCauseAnAbort
import org.scalatest.exceptions.NotAllowedException

/**
 * A sister class to <code>org.scalatest.FeatureSpec</code> that can pass a fixture object into its tests.
 *
 * <p>
 * The purpose of <code>FeatureSpec</code> and its subtraits is to facilitate writing tests in
 * a functional style. Some users may prefer writing tests in a functional style in general, but one
 * particular use case is parallel test execution (See <a href="../ParallelTestExecution.html">ParallelTestExecution</a>). To run
 * tests in parallel, your test class must
 * be thread safe, and a good way to make it thread safe is to make it functional. A good way to
 * write tests that need common fixtures in a functional style is to pass the fixture objects into the tests,
 * the style enabled by the <code>fixture.Suite</code> family of classes.
 * </p>
 *
 * <p>
 * Class <code>FeatureSpec</code> behaves similarly to class <code>org.scalatest.FeatureSpec</code>, except that tests may have a
 * fixture parameter. The type of the
 * fixture parameter is defined by the abstract <code>FixtureParam</code> type, which is a member of this class.
 * This class also has an abstract <code>withFixture</code> method. This <code>withFixture</code> method
 * takes a <code>OneArgTest</code>, which is a nested trait defined as a member of this class.
 * <code>OneArgTest</code> has an <code>apply</code> method that takes a <code>FixtureParam</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This class's <code>runTest</code> method delegates the actual running of each test to <code>withFixture</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture</code> method (abstract in this class) is responsible
 * for creating the fixture argument and passing it to the test function.
 * </p>
 * 
 * <p>
 * Subclasses of this class must, therefore, do three things differently from a plain old <code>org.scalatest.FeatureSpec</code>:
 * </p>
 * 
 * <ol>
 * <li>define the type of the fixture parameter by specifying type <code>FixtureParam</code></li>
 * <li>define the <code>withFixture(OneArgTest)</code> method</li>
 * <li>write tests that take a fixture parameter</li>
 * <li>(You can also define tests that don't take a fixture parameter.)</li>
 * </ol>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import collection.mutable.Stack
 * import java.util.NoSuchElementException
 *
 * class StackSpec extends fixture.FeatureSpec {
 *
 *   // 1. define type FixtureParam
 *   type FixtureParam = Stack[Int]
 *
 *   // 2. define the withFixture method
 *   def withFixture(test: OneArgTest) {
 *     val stack = new Stack[Int]
 *     stack.push(1)
 *     stack.push(2)
 *     test(stack) // "loan" the fixture to the test
 *   }
 *
 *   feature("Pushing a value onto a stack") {
 *
 *     // 3. write tests that take a fixture parameter
 *     scenario("User pushes a value") { stack =>
 *       stack.push(9)
 *       assert(stack.size === 3)
 *       assert(stack.head === 9)
 *     }
 *   }
 *
 *   feature("Popping a value off of a stack") {
 *
 *     scenario("User pops a value") { stack =>
 *       val top = stack.pop()
 *       assert(top === 2)
 *       assert(stack.size === 1)
 *     }
 *
 *     // 4. You can also write tests that don't take a fixture parameter.
 *     scenario("User calls pop on an empty stack") { () =>
 *       intercept[NoSuchElementException] {
 *         (new Stack[Int]).pop()
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * In the previous example, <code>withFixture</code> creates and initializes a stack, then invokes the test function, passing in
 * the stack.  In addition to setting up a fixture before a test, the <code>withFixture</code> method also allows you to
 * clean it up afterwards, if necessary. If you need to do some clean up that must happen even if a test
 * fails, you should invoke the test function from inside a <code>try</code> block and do the cleanup in a
 * <code>finally</code> clause, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * def withFixture(test: OneArgTest) {
 *   val resource = someResource.open() // set up the fixture
 *   try {
 *     test(resource) // if the test fails, test(...) will throw an exception
 *   }
 *   finally {
 *     // clean up the fixture no matter whether the test succeeds or fails
 *     resource.close()
 *   }
 * }
 * </pre>
 *
 * <p>
 * The reason you must perform cleanup in a <code>finally</code> clause is that <code>withFixture</code> is called by
 * <code>runTest</code>, which expects an exception to be thrown to indicate a failed test. Thus when you invoke
 * the <code>test</code> function, it may complete abruptly with an exception. The <code>finally</code> clause will
 * ensure the fixture cleanup happens as that exception propagates back up the call stack to <code>runTest</code>.
 * </p>
 *
 * <p>
 * If the fixture you want to pass into your tests consists of multiple objects, you will need to combine
 * them into one object to use this class. One good approach to passing multiple fixture objects is
 * to encapsulate them in a case class. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import scala.collection.mutable.ListBuffer
 *
 * class ExampleSpec extends fixture.FeatureSpec {
 *
 *   case class F(builder: StringBuilder, buffer: ListBuffer[String])
 *   type FixtureParam = F
 *
 *   def withFixture(test: OneArgTest) {
 *
 *     // Create needed mutable objects
 *     val stringBuilder = new StringBuilder("ScalaTest is ")
 *     val listBuffer = new ListBuffer[String]
 *
 *     // Invoke the test function, passing in the mutable objects
 *     test(F(stringBuilder, listBuffer))
 *   }
 *
 *   scenario("User finds testing easy") { f =>
 *     f.builder.append("easy!")
 *     assert(f.builder.toString === "ScalaTest is easy!")
 *     assert(f.buffer.isEmpty)
 *     f.buffer += "sweet"
 *   }
 *
 *   scenario("User finds testing fun") { f =>
 *     f.builder.append("fun!")
 *     assert(f.builder.toString === "ScalaTest is fun!")
 *     assert(f.buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <h2>Configuring fixtures and tests</h2>
 * 
 * <p>
 * Sometimes you may want to write tests that are configurable. For example, you may want to write
 * a suite of tests that each take an open temp file as a fixture, but whose file name is specified
 * externally so that the file name can be can be changed from run to run. To accomplish this
 * the <code>OneArgTest</code> trait has a <code>configMap</code>
 * method, which will return a <code>Map[String, Any]</code> from which configuration information may be obtained.
 * The <code>runTest</code> method of this class will pass a <code>OneArgTest</code> to <code>withFixture</code>
 * whose <code>configMap</code> method returns the <code>configMap</code> passed to <code>runTest</code>.
 * Here's an example in which the name of a temp file is taken from the passed <code>configMap</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class ExampleSpec extends fixture.FeatureSpec {
 *
 *   type FixtureParam = FileReader
 *   def withFixture(test: OneArgTest) {
 *
 *     require(
 *       test.configMap.contains("TempFileName"),
 *       "This suite requires a TempFileName to be passed in the configMap"
 *     )
 *
 *     // Grab the file name from the configMap
 *     val FileName = test.configMap("TempFileName").asInstanceOf[String]
 *
 *     // Set up the temp file needed by the test
 *     val writer = new FileWriter(FileName)
 *     try {
 *       writer.write("Hello, test!")
 *     }
 *     finally {
 *       writer.close()
 *     }
 *
 *     // Create the reader needed by the test
 *     val reader = new FileReader(FileName)
 *
 *     try {
 *       // Run the test using the temp file
 *       test(reader)
 *     }
 *     finally {
 *       // Close and delete the temp file
 *       reader.close()
 *       val file = new File(FileName)
 *       file.delete()
 *     }
 *   }
 *
 *   scenario("User reads the entire contents of a file") { reader =>
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 *
 *   scenario("User reads just the first char of a file") { reader =>
 *     assert(reader.read() === 'H')
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you want to pass into each test the entire <code>configMap</code> that was passed to <code>runTest</code>, you 
 * can mix in trait <code>ConfigMapFixture</code>. See the <a href="ConfigMapFixture.html">documentation
 * for <code>ConfigMapFixture</code></a> for the details, but here's a quick
 * example of how it looks:
 * </p>
 *
 * <pre class="stHighlight">
 *  import org.scalatest.fixture
 *  import org.scalatest.fixture.ConfigMapFixture
 *
 *  class ExampleSpec extends fixture.FeatureSpec with ConfigMapFixture {
 *
 *    feature("Test runs can be configured") {
 *
 *      scenario("User wants to be greeted by the config map") { configMap =>
 *        // Use the configMap passed to runTest in the test
 *        assert(configMap.contains("hello"))
 *      }
 *
 *      scenario("User wants the world from the config map") { configMap =>
 *        assert(configMap.contains("world"))
 *      }
 *    }
 *  }
 * </pre>
 *
 * <h2>Providing multiple fixtures</h2>
 *
 * <p>
 * If different tests in the same <code>FeatureSpec</code> need different shared fixtures, you can use the <em>loan pattern</em> to supply to
 * each test just the fixture or fixtures it needs. First select the most commonly used fixture objects and pass them in via the
 * <code>FixtureParam</code>. Then for each remaining fixture needed by multiple tests, create a <em>with&lt;fixture name&gt;</em>
 * method that takes a function you will use to pass the fixture to the test. Lasty, use the appropriate
 * <em>with&lt;fixture name&gt;</em> method or methods in each test.
 * </p>
 *
 * <p>
 * In the following example, the <code>FixtureParam</code> is set to <code>Map[String, Any]</code> by mixing in <code>ConfigMapFixture</code>.
 * The <code>withFixture</code> method in trait <code>ConfigMapFixture</code> will pass the config map to any test that needs it.
 * In addition, some tests in the following example need a <code>Stack[Int]</code> and others a <code>Stack[String]</code>.
 * The <code>withIntStack</code> method takes
 * care of supplying the <code>Stack[Int]</code> to those tests that need it, and the <code>withStringStack</code> method takes care
 * of supplying the <code>Stack[String]</code> fixture. Here's how it looks:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import org.scalatest.fixture.ConfigMapFixture
 * import collection.mutable.Stack
 * 
 * class StackSpec extends fixture.FeatureSpec with ConfigMapFixture {
 * 
 *   def withIntStack(test: Stack[Int] => Any) {
 *     val stack = new Stack[Int]
 *     stack.push(1)
 *     stack.push(2)
 *     test(stack) // "loan" the Stack[Int] fixture to the test
 *   }
 * 
 *   def withStringStack(test: Stack[String] => Any) {
 *     val stack = new Stack[String]
 *     stack.push("one")
 *     stack.push("two")
 *     test(stack) // "loan" the Stack[String] fixture to the test
 *   }
 * 
 *   scenario("User pops an Int value") { () => // This test doesn't need the configMap fixture, ...
 *     withIntStack { stack =>
 *       val top = stack.pop() // But it needs the Stack[Int] fixture.
 *       assert(top === 2)
 *       assert(stack.size === 1)
 *     }
 *   }
 * 
 *   scenario("User pushes and Int value") { configMap =>
 *     withIntStack { stack =>
 *       val iToPush = // This test uses the configMap fixture...
 *         configMap("IntToPush").toString.toInt
 *       stack.push(iToPush) // And also uses the Stack[Int] fixture.
 *       assert(stack.size === 3)
 *       assert(stack.head === iToPush)
 *     }
 *   }
 * 
 *   scenario("User pops a String value") { () => // This test doesn't need the configMap fixture, ...
 *     withStringStack { stack =>
 *       val top = stack.pop() // But it needs the Stack[String] fixture.
 *       assert(top === "two")
 *       assert(stack.size === 1)
 *     }
 *   }
 *
 *   scenario("User pushes a String value") { configMap =>
 *     withStringStack { stack =>
 *       val sToPush = // This test uses the configMap fixture...
 *         configMap("StringToPush").toString
 *       stack.push(sToPush) // And also uses the Stack[Int] fixture.
 *       assert(stack.size === 3)
 *       assert(stack.head === sToPush)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run the previous class in the Scala interpreter, you'll see:
 * </p>
 *
 * <pre class="stREPL">
 * scala> import org.scalatest._
 * import org.scalatest._
 *
 * scala> run(new StackSpec, configMap = Map("IntToPush" -> 9, "StringToPush" -> "nine"))
 * <span class="stGreen">StackSpec:
 * - User pops an Int value
 * - User pushes an Int value
 * - User pops a String value
 * - User pushes a String value</span>
 * </pre>
 *
 * @author Bill Venners
 */
abstract class FeatureSpec extends FeatureSpecLike

