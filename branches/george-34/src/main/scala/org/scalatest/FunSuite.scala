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

import scala.collection.immutable.ListSet
import java.util.ConcurrentModificationException
import java.util.concurrent.atomic.AtomicReference
import org.scalatest.StackDepthExceptionHelper.getStackDepth
import org.scalatest.events._
import Suite.anErrorThatShouldCauseAnAbort
import Suite.checkRunTestParamsForNull

/**
 * A suite of tests in which each test is represented as a function value. The &#8220;<code>Fun</code>&#8221; in <code>FunSuite</code> stands
 * for &#8220;function.&#8221; Here's an example <code>FunSuite</code>:
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 *
 * class MySuite extends FunSuite {
 *
 *   test("addition") {
 *     val sum = 1 + 1
 *     assert(sum === 2)
 *     assert(sum + 2 === 4)
 *   }
 *
 *   test("subtraction") {
 *     val diff = 4 - 1
 *     assert(diff === 3)
 *     assert(diff - 2 === 1)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> {
 * <br />  test(<span class="stQuotedString">"addition"</span>) {
 *     <span class="stReserved">val</span> sum = <span class="stLiteral">1</span> + <span class="stLiteral">1</span>
 *     assert(sum === <span class="stLiteral">2</span>)
 *     assert(sum + <span class="stLiteral">2</span> === <span class="stLiteral">4</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"subtraction"</span>) {
 *     <span class="stReserved">val</span> diff = <span class="stLiteral">4</span> - <span class="stLiteral">1</span>
 *     assert(diff === <span class="stLiteral">3</span>)
 *     assert(diff - <span class="stLiteral">2</span> === <span class="stLiteral">1</span>)
 *   }
 * }
 * </pre>
 *
 * <p>
 * &#8220;<code>test</code>&#8221; is a method, defined in <code>FunSuite</code>, which will be invoked
 * by the primary constructor of <code>MySuite</code>. You specify the name of the test as
 * a string between the parentheses, and the test code itself between curly braces.
 * The test code is a function passed as a by-name parameter to <code>test</code>, which registers
 * it for later execution. One benefit of <code>FunSuite</code> compared to <code>Suite</code> is you need not name all your
 * tests starting with &#8220;<code>test</code>.&#8221; In addition, you can more easily give long names to
 * your tests, because you need not encode them in camel case, as most people would tend to do
 * for test method names.
 * </p>
 *
 * <p>
 * A <code>FunSuite</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Tests can only be registered with the <code>test</code> method while the <code>FunSuite</code> is
 * in its registration phase. Any attempt to register a test after the <code>FunSuite</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>FunSuite</code>,
 * will be met with a thrown <code>TestRegistrationClosedException</code>. The recommended style
 * of using <code>FunSuite</code> is to register tests during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <p>
 * See also: <a href="http://www.scalatest.org/getting_started_with_fun_suite" target="_blank">Getting started with <code>FunSuite</code>.</a>
 * </p>
 * 
 * <a name="SharedFixtures"></a><h2>Shared fixtures</h2>
 *
 *
 * <p>
 * A test <em>fixture</em> is objects or other artifacts (such as files, sockets, database
 * connections, etc.) used by tests to do their work. You can use fixtures in
 * <code>FunSuite</code>s with the same approaches suggested for <code>Suite</code> in
 * its documentation. The same text that appears in the test fixture
 * section of <code>Suite</code>'s documentation is repeated here, with examples changed from
 * <code>Suite</code> to <code>FunSuite</code>.
 * </p>
 *
 * <p>
 * If a fixture is used by only one test, then the definitions of the fixture objects can
 * be local to the test function, such as the objects assigned to <code>sum</code> and <code>diff</code> in the
 * previous <code>MySuite</code> examples. If multiple tests need to share a fixture, the best approach
 * is to assign them to instance variables. Here's a (very contrived) example, in which the object assigned
 * to <code>shared</code> is used by multiple test functions:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 *
 * class MySuite extends FunSuite {
 *
 *   // Sharing immutable fixture objects via instance variables
 *   val shared = 5
 *
 *   test("addition") {
 *     val sum = 2 + 3
 *     assert(sum === shared)
 *   }
 *
 *   test("subtraction") {
 *     val diff = 7 - 2
 *     assert(diff === shared)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> {
 * <br />  <span class="stLineComment">// Sharing immutable fixture objects via instance variables</span>
 *   <span class="stReserved">val</span> shared = <span class="stLiteral">5</span>
 * <br />  test(<span class="stQuotedString">"addition"</span>) {
 *     <span class="stReserved">val</span> sum = <span class="stLiteral">2</span> + <span class="stLiteral">3</span>
 *     assert(sum === shared)
 *   }
 * <br />  test(<span class="stQuotedString">"subtraction"</span>) {
 *     <span class="stReserved">val</span> diff = <span class="stLiteral">7</span> - <span class="stLiteral">2</span>
 *     assert(diff === shared)
 *   }
 * }
 * </pre>
 *
 * <p>
 * In some cases, however, shared <em>mutable</em> fixture objects may be changed by tests such that
 * they need to be recreated or reinitialized before each test. Shared resources such
 * as files or database connections may also need to be created and initialized before,
 * and cleaned up after, each test. JUnit offers methods <code>setUp</code> and
 * <code>tearDown</code> for this purpose. In ScalaTest, you can use the <code>BeforeAndAfterEach</code> trait,
 * which will be described later, to implement an approach similar to JUnit's <code>setUp</code>
 * and <code>tearDown</code>, however, this approach often involves reassigning <code>var</code>s
 * between tests. Before going that route, you should consider some approaches that
 * avoid <code>var</code>s. One approach is to write one or more <em>create-fixture</em> methods
 * that return a new instance of a needed object (or a tuple or case class holding new instances of
 * multiple objects) each time it is called. You can then call a create-fixture method at the beginning of each
 * test that needs the fixture, storing the fixture object or objects in local variables. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 * import scala.collection.mutable.ListBuffer
 *
 * class MySuite extends FunSuite {
 *
 *   // create objects needed by tests and return as a tuple
 *   def createFixture = (
 *     new StringBuilder("ScalaTest is "),
 *     new ListBuffer[String]
 *   )
 *
 *   test("easy") {
 *     val (builder, lbuf) = createFixture
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(lbuf.isEmpty)
 *     lbuf += "sweet"
 *   }
 *
 *   test("fun") {
 *     val (builder, lbuf) = createFixture
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(lbuf.isEmpty)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <span class="stReserved">import</span> scala.collection.mutable.ListBuffer
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> {
 * <br />  <span class="stLineComment">// create objects needed by tests and return as a tuple</span>
 *   <span class="stReserved">def</span> createFixture = (
 *     <span class="stReserved">new</span> <span class="stType">StringBuilder</span>(<span class="stQuotedString">"ScalaTest is "</span>),
 *     <span class="stReserved">new</span> <span class="stType">ListBuffer[String]</span>
 *   )
 * <br />  test(<span class="stQuotedString">"easy"</span>) {
 *     <span class="stReserved">val</span> (builder, lbuf) = createFixture
 *     builder.append(<span class="stQuotedString">"easy!"</span>)
 *     assert(builder.toString === <span class="stQuotedString">"ScalaTest is easy!"</span>)
 *     assert(lbuf.isEmpty)
 *     lbuf += <span class="stQuotedString">"sweet"</span>
 *   }
 * <br />  test(<span class="stQuotedString">"fun"</span>) {
 *     <span class="stReserved">val</span> (builder, lbuf) = createFixture
 *     builder.append(<span class="stQuotedString">"fun!"</span>)
 *     assert(builder.toString === <span class="stQuotedString">"ScalaTest is fun!"</span>)
 *     assert(lbuf.isEmpty)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If different tests in the same <code>FunSuite</code> require different fixtures, you can create multiple create-fixture methods and
 * call the method (or methods) needed by each test at the begining of the test. If every test requires the same set of
 * mutable fixture objects, one other approach you can take is make them simply <code>val</code>s and mix in trait
 * <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>.  If you mix in <code>OneInstancePerTest</code>, each test
 * will be run in its own instance of the <code>FunSuite</code>, similar to the way JUnit tests are executed.
 * </p>
 *
 * <p>
 * Although the create-fixture and <code>OneInstancePerTest</code> approaches take care of setting up a fixture before each
 * test, they don't address the problem of cleaning up a fixture after the test completes. In this situation,
 * one option is to mix in the <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a> trait.
 * <code>BeforeAndAfterEach</code>'s <code>beforeEach</code> method will be run before, and its <code>afterEach</code>
 * method after, each test (like JUnit's <code>setUp</code>  and <code>tearDown</code>
 * methods, respectively). 
 * For example, you could create a temporary file before each test, and delete it afterwords, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 * import org.scalatest.BeforeAndAfterEach
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class MySuite extends FunSuite with BeforeAndAfterEach {
 *
 *   private val FileName = "TempFile.txt"
 *   private var reader: FileReader = _
 *
 *   // Set up the temp file needed by the test
 *   override def beforeEach() {
 *     val writer = new FileWriter(FileName)
 *     try {
 *       writer.write("Hello, test!")
 *     }
 *     finally {
 *       writer.close()
 *     }
 *
 *     // Create the reader needed by the test
 *     reader = new FileReader(FileName)
 *   }
 *
 *   // Close and delete the temp file
 *   override def afterEach() {
 *     reader.close()
 *     val file = new File(FileName)
 *     file.delete()
 *   }
 *
 *   test("reading from the temp file") {
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 *
 *   test("first char of the temp file") {
 *     assert(reader.read() === 'H')
 *   }
 * 
 *   test("without a fixture") {
 *     assert(1 + 1 === 2)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <span class="stReserved">import</span> org.scalatest.BeforeAndAfterEach
 * <span class="stReserved">import</span> java.io.FileReader
 * <span class="stReserved">import</span> java.io.FileWriter
 * <span class="stReserved">import</span> java.io.File
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> <span class="stReserved">with</span> <span class="stType">BeforeAndAfterEach</span> {
 * <br />  <span class="stReserved">private</span> <span class="stReserved">val</span> <span class="stType">FileName</span> = <span class="stQuotedString">"TempFile.txt"</span>
 *   <span class="stReserved">private</span> <span class="stReserved">var</span> reader: <span class="stType">FileReader</span> = _
 * <br />  <span class="stLineComment">// Set up the temp file needed by the test</span>
 *   <span class="stReserved">override</span> <span class="stReserved">def</span> beforeEach() {
 *     <span class="stReserved">val</span> writer = <span class="stReserved">new</span> <span class="stType">FileWriter</span>(<span class="stType">FileName</span>)
 *     <span class="stReserved">try</span> {
 *       writer.write(<span class="stQuotedString">"Hello, test!"</span>)
 *     }
 *     <span class="stReserved">finally</span> {
 *       writer.close()
 *     }
 * <br />    <span class="stLineComment">// Create the reader needed by the test</span>
 *     reader = <span class="stReserved">new</span> <span class="stType">FileReader</span>(<span class="stType">FileName</span>)
 *   }
 * <br />  <span class="stLineComment">// Close and delete the temp file</span>
 *   <span class="stReserved">override</span> <span class="stReserved">def</span> afterEach() {
 *     reader.close()
 *     <span class="stReserved">val</span> file = <span class="stReserved">new</span> <span class="stType">File</span>(<span class="stType">FileName</span>)
 *     file.delete()
 *   }
 * <br />  test(<span class="stQuotedString">"reading from the temp file"</span>) {
 *     <span class="stReserved">var</span> builder = <span class="stReserved">new</span> <span class="stType">StringBuilder</span>
 *     <span class="stReserved">var</span> c = reader.read()
 *     <span class="stReserved">while</span> (c != -<span class="stLiteral">1</span>) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === <span class="stQuotedString">"Hello, test!"</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"first char of the temp file"</span>) {
 *     assert(reader.read() === <span class="stQuotedString">'H'</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"without a fixture"</span>) {
 *     assert(<span class="stLiteral">1</span> + <span class="stLiteral">1</span> === <span class="stLiteral">2</span>)
 *   }
 * }
 * </pre>
 *
 * <p>
 * In this example, the instance variable <code>reader</code> is a <code>var</code>, so
 * it can be reinitialized between tests by the <code>beforeEach</code> method.
 * </p>
 * 
 * <p>
 * Although the <code>BeforeAndAfterEach</code> approach should be familiar to the users of most
 * test other frameworks, ScalaTest provides another alternative that also allows you to perform cleanup
 * after each test: overriding <code>withFixture(NoArgTest)</code>.
 * To execute each test, <code>Suite</code>'s implementation of the <code>runTest</code> method wraps an invocation
 * of the appropriate test method in a no-arg function. <code>runTest</code> passes that test function to the <code>withFixture(NoArgTest)</code>
 * method, which is responsible for actually running the test by invoking the function. <code>Suite</code>'s
 * implementation of <code>withFixture(NoArgTest)</code> simply invokes the function, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Default implementation
 * protected def withFixture(test: NoArgTest) {
 *   test()
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stLineComment">// Default implementation</span>
 * <span class="stReserved">protected</span> <span class="stReserved">def</span> withFixture(test: <span class="stType">NoArgTest</span>) {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * The <code>withFixture(NoArgTest)</code> method exists so that you can override it and set a fixture up before, and clean it up after, each test.
 * Thus, the previous temp file example could also be implemented without mixing in <code>BeforeAndAfterEach</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 * import org.scalatest.BeforeAndAfterEach
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class MySuite extends FunSuite {
 *
 *   private var reader: FileReader = _
 *
 *   override def withFixture(test: NoArgTest) {
 *
 *     val FileName = "TempFile.txt"
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
 *     reader = new FileReader(FileName)
 *
 *     try {
 *       test() // Invoke the test function
 *     }
 *     finally {
 *       // Close and delete the temp file
 *       reader.close()
 *       val file = new File(FileName)
 *       file.delete()
 *     }
 *   }
 *
 *   test("reading from the temp file") {
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 *
 *   test("first char of the temp file") {
 *     assert(reader.read() === 'H')
 *   }
 * 
 *   test("without a fixture") {
 *     assert(1 + 1 === 2)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <span class="stReserved">import</span> org.scalatest.BeforeAndAfterEach
 * <span class="stReserved">import</span> java.io.FileReader
 * <span class="stReserved">import</span> java.io.FileWriter
 * <span class="stReserved">import</span> java.io.File
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> {
 * <br />  <span class="stReserved">private</span> <span class="stReserved">var</span> reader: <span class="stType">FileReader</span> = _
 * <br />  <span class="stReserved">override</span> <span class="stReserved">def</span> withFixture(test: <span class="stType">NoArgTest</span>) {
 * <br />    <span class="stReserved">val</span> <span class="stType">FileName</span> = <span class="stQuotedString">"TempFile.txt"</span>
 * <br />    <span class="stLineComment">// Set up the temp file needed by the test</span>
 *     <span class="stReserved">val</span> writer = <span class="stReserved">new</span> <span class="stType">FileWriter</span>(<span class="stType">FileName</span>)
 *     <span class="stReserved">try</span> {
 *       writer.write(<span class="stQuotedString">"Hello, test!"</span>)
 *     }
 *     <span class="stReserved">finally</span> {
 *       writer.close()
 *     }
 * <br />    <span class="stLineComment">// Create the reader needed by the test</span>
 *     reader = <span class="stReserved">new</span> <span class="stType">FileReader</span>(<span class="stType">FileName</span>)
 * <br />    <span class="stReserved">try</span> {
 *       test() <span class="stLineComment">// Invoke the test function</span>
 *     }
 *     <span class="stReserved">finally</span> {
 *       <span class="stLineComment">// Close and delete the temp file</span>
 *       reader.close()
 *       <span class="stReserved">val</span> file = <span class="stReserved">new</span> <span class="stType">File</span>(<span class="stType">FileName</span>)
 *       file.delete()
 *     }
 *   }
 * <br />  test(<span class="stQuotedString">"reading from the temp file"</span>) {
 *     <span class="stReserved">var</span> builder = <span class="stReserved">new</span> <span class="stType">StringBuilder</span>
 *     <span class="stReserved">var</span> c = reader.read()
 *     <span class="stReserved">while</span> (c != -<span class="stLiteral">1</span>) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === <span class="stQuotedString">"Hello, test!"</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"first char of the temp file"</span>) {
 *     assert(reader.read() === <span class="stQuotedString">'H'</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"without a fixture"</span>) {
 *     assert(<span class="stLiteral">1</span> + <span class="stLiteral">1</span> === <span class="stLiteral">2</span>)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you prefer to keep your test classes immutable, one final variation is to use the
 * <a href="fixture/FixtureFunSuite.html"><code>FixtureFunSuite</code></a> trait from the
 * <code>org.scalatest.fixture</code> package.  Tests in an <code>org.scalatest.fixture.FixtureFunSuite</code> can have a fixture
 * object passed in as a parameter. You must indicate the type of the fixture object
 * by defining the <code>Fixture</code> type member and define a <code>withFixture</code> method that takes a <em>one-arg</em> test function.
 * (A <code>FixtureFunSuite</code> has two overloaded <code>withFixture</code> methods, therefore, one that takes a <code>OneArgTest</code>
 * and the other, inherited from <code>Suite</code>, that takes a <code>NoArgTest</code>.)
 * Inside the <code>withFixture(OneArgTest)</code> method, you create the fixture, pass it into the test function, then perform any
 * necessary cleanup after the test function returns. Instead of invoking each test directly, a <code>FixtureFunSuite</code> will
 * pass a function that invokes the code of a test to <code>withFixture(OneArgTest)</code>. Your <code>withFixture(OneArgTest)</code> method, therefore,
 * is responsible for actually running the code of the test by invoking the test function.
 * For example, you could pass the temp file reader fixture to each test that needs it
 * by overriding the <code>withFixture(OneArgTest)</code> method of a <code>FixtureFunSuite</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture.FixtureFunSuite
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class MySuite extends FixtureFunSuite {
 *
 *   type FixtureParam = FileReader
 *
 *   def withFixture(test: OneArgTest) {
 *
 *     val FileName = "TempFile.txt"
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
 *   test("reading from the temp file") { reader =>
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === "Hello, test!")
 *   }
 * 
 *   test("first char of the temp file") { reader =>
 *     assert(reader.read() === 'H')
 *   }
 * 
 *   test("without a fixture") { () => 
 *     assert(1 + 1 === 2)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.fixture.FixtureFunSuite
 * <span class="stReserved">import</span> java.io.FileReader
 * <span class="stReserved">import</span> java.io.FileWriter
 * <span class="stReserved">import</span> java.io.File
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FixtureFunSuite</span> {
 * <br />  <span class="stReserved">type</span> <span class="stType">FixtureParam</span> = <span class="stType">FileReader</span>
 * <br />  <span class="stReserved">def</span> withFixture(test: <span class="stType">OneArgTest</span>) {
 * <br />    <span class="stReserved">val</span> <span class="stType">FileName</span> = <span class="stQuotedString">"TempFile.txt"</span>
 * <br />    <span class="stLineComment">// Set up the temp file needed by the test</span>
 *     <span class="stReserved">val</span> writer = <span class="stReserved">new</span> <span class="stType">FileWriter</span>(<span class="stType">FileName</span>)
 *     <span class="stReserved">try</span> {
 *       writer.write(<span class="stQuotedString">"Hello, test!"</span>)
 *     }
 *     <span class="stReserved">finally</span> {
 *       writer.close()
 *     }
 * <br />    <span class="stLineComment">// Create the reader needed by the test</span>
 *     <span class="stReserved">val</span> reader = <span class="stReserved">new</span> <span class="stType">FileReader</span>(<span class="stType">FileName</span>)
 * <br />    <span class="stReserved">try</span> {
 *       <span class="stLineComment">// Run the test using the temp file</span>
 *       test(reader)
 *     }
 *     <span class="stReserved">finally</span> {
 *       <span class="stLineComment">// Close and delete the temp file</span>
 *       reader.close()
 *       <span class="stReserved">val</span> file = <span class="stReserved">new</span> <span class="stType">File</span>(<span class="stType">FileName</span>)
 *       file.delete()
 *     }
 *   }
 * <br />  test(<span class="stQuotedString">"reading from the temp file"</span>) { reader =>
 *     <span class="stReserved">var</span> builder = <span class="stReserved">new</span> <span class="stType">StringBuilder</span>
 *     <span class="stReserved">var</span> c = reader.read()
 *     <span class="stReserved">while</span> (c != -<span class="stLiteral">1</span>) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     assert(builder.toString === <span class="stQuotedString">"Hello, test!"</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"first char of the temp file"</span>) { reader =>
 *     assert(reader.read() === <span class="stQuotedString">'H'</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"without a fixture"</span>) { () => 
 *     assert(<span class="stLiteral">1</span> + <span class="stLiteral">1</span> === <span class="stLiteral">2</span>)
 *   }
 * }
 * </pre>
 *
 * <p>
 * It is worth noting that the only difference in the test code between the mutable
 * <code>BeforeAndAfterEach</code> approach shown here and the immutable <code>FixtureFunSuite</code>
 * approach shown previously is that two of the <code>FixtureFunSuite</code>'s test functions take a <code>FileReader</code> as
 * a parameter via the "<code>reader =></code>" at the beginning of the function. Otherwise the test code is identical.
 * One benefit of the explicit parameter is that, as demonstrated
 * by the "<code>without a fixture</code>" test, a <code>FixtureFunSuite</code>
 * test need not take the fixture. So you can have some tests that take a fixture, and others that don't.
 * In this case, the <code>FixtureFunSuite</code> provides documentation indicating which
 * tests use the fixture and which don't, whereas the <code>BeforeAndAfterEach</code> approach does not.
 * (If you have want to combine tests that take different fixture types in the same <code>FunSuite</code>, you can
 * use <a href="fixture/MultipleFixtureFunSuite.html">MultipleFixtureFunSuite</a>.)
 * </p>
 *
 * <p>
 * If you want to execute code before and after all tests (and nested suites) in a suite, such
 * want to execute code before and after all tests (and nested suites) in a suite, such
 * as you could do with <code>@BeforeClass</code> and <code>@AfterClass</code>
 * annotations in JUnit 4, you can use the <code>beforeAll</code> and <code>afterAll</code>
 * methods of <code>BeforeAndAfterAll</code>. See the documentation for <code>BeforeAndAfterAll</code> for
 * an example.
 * </p>
 *
 * <a name="SharedTests"></a><h2>Shared tests</h2>
 *
 * <p>
 * Sometimes you may want to run the same test code on different fixture objects. In other words, you may want to write tests that are "shared"
 * by different fixture objects.
 * To accomplish this in a <code>FunSuite</code>, you first place shared tests in
 * <em>behavior functions</em>. These behavior functions will be
 * invoked during the construction phase of any <code>FunSuite</code> that uses them, so that the tests they contain will
 * be registered as tests in that <code>FunSuite</code>.
 * For example, given this stack class:
 * </p>
 *
 * <pre class="stHighlight">
 * import scala.collection.mutable.ListBuffer
 * 
 * class Stack[T] {
 *
 *   val MAX = 10
 *   private var buf = new ListBuffer[T]
 *
 *   def push(o: T) {
 *     if (!full)
 *       o +: buf
 *     else
 *       throw new IllegalStateException("can't push onto a full stack")
 *   }
 *
 *   def pop(): T = {
 *     if (!empty)
 *       buf.remove(0)
 *     else
 *       throw new IllegalStateException("can't pop an empty stack")
 *   }
 *
 *   def peek: T = {
 *     if (!empty)
 *       buf(0)
 *     else
 *       throw new IllegalStateException("can't pop an empty stack")
 *   }
 *
 *   def full: Boolean = buf.size == MAX
 *   def empty: Boolean = buf.size == 0
 *   def size = buf.size
 *
 *   override def toString = buf.mkString("Stack(", ", ", ")")
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> scala.collection.mutable.ListBuffer
 * <br /><span class="stReserved">class</span> <span class="stType">Stack[T]</span> {
 * <br />  <span class="stReserved">val</span> MAX = <span class="stLiteral">10</span>
 *   <span class="stReserved">private</span> <span class="stReserved">var</span> buf = <span class="stReserved">new</span> <span class="stType">ListBuffer[T]</span>
 * <br />  <span class="stReserved">def</span> push(o: T) {
 *     <span class="stReserved">if</span> (!full)
 *       o +: buf
 *     <span class="stReserved">else</span>
 *       <span class="stReserved">throw</span> <span class="stReserved">new</span> <span class="stType">IllegalStateException</span>(<span class="stQuotedString">"can't push onto a full stack"</span>)
 *   }
 * <br />  <span class="stReserved">def</span> pop(): T = {
 *     <span class="stReserved">if</span> (!empty)
 *       buf.remove(<span class="stLiteral">0</span>)
 *     <span class="stReserved">else</span>
 *       <span class="stReserved">throw</span> <span class="stReserved">new</span> <span class="stType">IllegalStateException</span>(<span class="stQuotedString">"can't pop an empty stack"</span>)
 *   }
 * <br />  <span class="stReserved">def</span> peek: T = {
 *     <span class="stReserved">if</span> (!empty)
 *       buf(<span class="stLiteral">0</span>)
 *     <span class="stReserved">else</span>
 *       <span class="stReserved">throw</span> <span class="stReserved">new</span> <span class="stType">IllegalStateException</span>(<span class="stQuotedString">"can't pop an empty stack"</span>)
 *   }
 * <br />  <span class="stReserved">def</span> full: <span class="stType">Boolean</span> = buf.size == MAX
 *   <span class="stReserved">def</span> empty: <span class="stType">Boolean</span> = buf.size == <span class="stLiteral">0</span>
 *   <span class="stReserved">def</span> size = buf.size
 * <br />  <span class="stReserved">override</span> <span class="stReserved">def</span> toString = buf.mkString(<span class="stQuotedString">"Stack("</span>, <span class="stQuotedString">", "</span>, <span class="stQuotedString">")"</span>)
 * }
 * </pre>
 *
 * <p>
 * You may want to test the <code>Stack</code> class in different states: empty, full, with one item, with one item less than capacity,
 * <em>etc</em>. You may find you have several tests that make sense any time the stack is non-empty. Thus you'd ideally want to run
 * those same tests for three stack fixture objects: a full stack, a stack with a one item, and a stack with one item less than
 * capacity. With shared tests, you can factor these tests out into a behavior function, into which you pass the
 * stack fixture to use when running the tests. So in your <code>FunSuite</code> for stack, you'd invoke the
 * behavior function three times, passing in each of the three stack fixtures so that the shared tests are run for all three fixtures.
 * </p>
 *
 * <p>
 * You can define a behavior function that encapsulates these shared tests inside the <code>FunSuite</code> that uses them. If they are shared
 * between different <code>FunSuite</code>s, however, you could also define them in a separate trait that is mixed into
 * each <code>FunSuite</code> that uses them.
 * <a name="StackBehaviors">For</a> example, here the <code>nonEmptyStack</code> behavior function (in this case, a
 * behavior <em>method</em>) is defined in a trait along with another
 * method containing shared tests for non-full stacks:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 * 
 * trait FunSuiteStackBehaviors { this: FunSuite =>
 * 
 *   def nonEmptyStack(createNonEmptyStack: => Stack[Int], lastItemAdded: Int) {
 * 
 *     test("empty is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
 *       val stack = createNonEmptyStack
 *       assert(!stack.empty)
 *     }
 * 
 *     test("peek is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
 *       val stack = createNonEmptyStack
 *       val size = stack.size
 *       assert(stack.peek === lastItemAdded)
 *       assert(stack.size === size)
 *     }
 * 
 *     test("pop is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
 *       val stack = createNonEmptyStack
 *       val size = stack.size
 *       assert(stack.pop === lastItemAdded)
 *       assert(stack.size === size - 1)
 *     }
 *   }
 *   
 *   def nonFullStack(createNonFullStack: => Stack[Int]) {
 *       
 *     test("full is invoked on this non-full stack: " + createNonFullStack.toString) {
 *       val stack = createNonFullStack
 *       assert(!stack.full)
 *     }
 *       
 *     test("push is invoked on this non-full stack: " + createNonFullStack.toString) {
 *       val stack = createNonFullStack
 *       val size = stack.size
 *       stack.push(7)
 *       assert(stack.size === size + 1)
 *       assert(stack.peek === 7)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <br /><span class="stReserved">trait</span> <span class="stType">FunSuiteStackBehaviors</span> { <span class="stReserved">this</span>: <span class="stType">FunSuite</span> =>
 * <br />  <span class="stReserved">def</span> nonEmptyStack(createNonEmptyStack: => <span class="stType">Stack[Int]</span>, lastItemAdded: <span class="stType">Int</span>) {
 * <br />    test(<span class="stQuotedString">"empty is invoked on this non-empty stack: "</span> + createNonEmptyStack.toString) {
 *       <span class="stReserved">val</span> stack = createNonEmptyStack
 *       assert(!stack.empty)
 *     }
 * <br />    test(<span class="stQuotedString">"peek is invoked on this non-empty stack: "</span> + createNonEmptyStack.toString) {
 *       <span class="stReserved">val</span> stack = createNonEmptyStack
 *       <span class="stReserved">val</span> size = stack.size
 *       assert(stack.peek === lastItemAdded)
 *       assert(stack.size === size)
 *     }
 * <br />    test(<span class="stQuotedString">"pop is invoked on this non-empty stack: "</span> + createNonEmptyStack.toString) {
 *       <span class="stReserved">val</span> stack = createNonEmptyStack
 *       <span class="stReserved">val</span> size = stack.size
 *       assert(stack.pop === lastItemAdded)
 *       assert(stack.size === size - <span class="stLiteral">1</span>)
 *     }
 *   }
 * <br />  <span class="stReserved">def</span> nonFullStack(createNonFullStack: => <span class="stType">Stack[Int]</span>) {
 * <br />    test(<span class="stQuotedString">"full is invoked on this non-full stack: "</span> + createNonFullStack.toString) {
 *       <span class="stReserved">val</span> stack = createNonFullStack
 *       assert(!stack.full)
 *     }
 * <br />    test(<span class="stQuotedString">"push is invoked on this non-full stack: "</span> + createNonFullStack.toString) {
 *       <span class="stReserved">val</span> stack = createNonFullStack
 *       <span class="stReserved">val</span> size = stack.size
 *       stack.push(<span class="stLiteral">7</span>)
 *       assert(stack.size === size + <span class="stLiteral">1</span>)
 *       assert(stack.peek === <span class="stLiteral">7</span>)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Given these behavior functions, you could invoke them directly, but <code>FunSuite</code> offers a DSL for the purpose,
 * which looks like this:
 * </p>
 *
 * <pre class="stHighlight">
 * testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * testsFor(nonFullStack(stackWithOneItem))
 * </pre><pre class="stHighlighted">
 * testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * testsFor(nonFullStack(stackWithOneItem))
 * </pre>
 *
 * <p>
 * If you prefer to use an imperative style to change fixtures, for example by mixing in <code>BeforeAndAfterEach</code> and
 * reassigning a <code>stack</code> <code>var</code> in <code>beforeEach</code>, you could write your behavior functions
 * in the context of that <code>var</code>, which means you wouldn't need to pass in the stack fixture because it would be
 * in scope already inside the behavior function. In that case, your code would look like this:
 * </p>
 *
 * <pre class="stHighlight">
 * testsFor(nonEmptyStack) // assuming lastValuePushed is also in scope inside nonEmptyStack
 * testsFor(nonFullStack)
 * </pre><pre class="stHighlighted">
 * testsFor(nonEmptyStack) <span class="stLineComment">// assuming lastValuePushed is also in scope inside nonEmptyStack</span>
 * testsFor(nonFullStack)
 * </pre>
 *
 * <p>
 * The recommended style, however, is the functional, pass-all-the-needed-values-in style. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 * 
 * class StackFunSuite extends FunSuite with FunSuiteStackBehaviors {
 * 
 *   // Stack fixture creation methods
 *   def emptyStack = new Stack[Int]
 *  
 *   def fullStack = {
 *     val stack = new Stack[Int]
 *     for (i <- 0 until stack.MAX)
 *       stack.push(i)
 *     stack
 *   }
 *  
 *   def stackWithOneItem = {
 *     val stack = new Stack[Int]
 *     stack.push(9)
 *     stack
 *   }
 *  
 *   def stackWithOneItemLessThanCapacity = {
 *     val stack = new Stack[Int]
 *     for (i <- 1 to 9)
 *       stack.push(i)
 *     stack
 *   }
 *  
 *   val lastValuePushed = 9
 *  
 *   test("empty is invoked on an empty stack") {
 *     val stack = emptyStack
 *     assert(stack.empty)
 *   }
 *
 *   test("peek is invoked on an empty stack") {
 *     val stack = emptyStack
 *     intercept[IllegalStateException] {
 *       stack.peek
 *     }
 *   }
 *
 *   test("pop is invoked on an empty stack") {
 *     val stack = emptyStack
 *     intercept[IllegalStateException] {
 *       emptyStack.pop
 *     }
 *   }
 *
 *   testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 *   testsFor(nonFullStack(stackWithOneItem))
 *
 *   testsFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
 *   testsFor(nonFullStack(stackWithOneItemLessThanCapacity))
 *
 *   test("full is invoked on a full stack") {
 *     val stack = fullStack
 *     assert(stack.full)
 *   }
 *
 *   testsFor(nonEmptyStack(fullStack, lastValuePushed))
 *
 *   test("push is invoked on a full stack") {
 *     val stack = fullStack
 *     intercept[IllegalStateException] {
 *       stack.push(10)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <br /><span class="stReserved">class</span> <span class="stType">StackFunSuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> <span class="stReserved">with</span> <span class="stType">FunSuiteStackBehaviors</span> {
 * <br />  <span class="stLineComment">// Stack fixture creation methods</span>
 *   <span class="stReserved">def</span> emptyStack = <span class="stReserved">new</span> <span class="stType">Stack[Int]</span>
 * <br />  <span class="stReserved">def</span> fullStack = {
 *     <span class="stReserved">val</span> stack = <span class="stReserved">new</span> <span class="stType">Stack[Int]</span>
 *     <span class="stReserved">for</span> (i <- <span class="stLiteral">0</span> until stack.MAX)
 *       stack.push(i)
 *     stack
 *   }
 * <br />  <span class="stReserved">def</span> stackWithOneItem = {
 *     <span class="stReserved">val</span> stack = <span class="stReserved">new</span> <span class="stType">Stack[Int]</span>
 *     stack.push(<span class="stLiteral">9</span>)
 *     stack
 *   }
 * <br />  <span class="stReserved">def</span> stackWithOneItemLessThanCapacity = {
 *     <span class="stReserved">val</span> stack = <span class="stReserved">new</span> <span class="stType">Stack[Int]</span>
 *     <span class="stReserved">for</span> (i <- <span class="stLiteral">1</span> to <span class="stLiteral">9</span>)
 *       stack.push(i)
 *     stack
 *   }
 * <br />  <span class="stReserved">val</span> lastValuePushed = <span class="stLiteral">9</span>
 * <br />  test(<span class="stQuotedString">"empty is invoked on an empty stack"</span>) {
 *     <span class="stReserved">val</span> stack = emptyStack
 *     assert(stack.empty)
 *   }
 * <br />  test(<span class="stQuotedString">"peek is invoked on an empty stack"</span>) {
 *     <span class="stReserved">val</span> stack = emptyStack
 *     intercept[<span class="stType">IllegalStateException</span>] {
 *       stack.peek
 *     }
 *   }
 * <br />  test(<span class="stQuotedString">"pop is invoked on an empty stack"</span>) {
 *     <span class="stReserved">val</span> stack = emptyStack
 *     intercept[<span class="stType">IllegalStateException</span>] {
 *       emptyStack.pop
 *     }
 *   }
 * <br />  testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 *   testsFor(nonFullStack(stackWithOneItem))
 * <br />  testsFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
 *   testsFor(nonFullStack(stackWithOneItemLessThanCapacity))
 * <br />  test(<span class="stQuotedString">"full is invoked on a full stack"</span>) {
 *     <span class="stReserved">val</span> stack = fullStack
 *     assert(stack.full)
 *   }
 * <br />  testsFor(nonEmptyStack(fullStack, lastValuePushed))
 * <br />  test(<span class="stQuotedString">"push is invoked on a full stack"</span>) {
 *     <span class="stReserved">val</span> stack = fullStack
 *     intercept[<span class="stType">IllegalStateException</span>] {
 *       stack.push(<span class="stLiteral">10</span>)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you load these classes into the Scala interpreter (with scalatest's JAR file on the class path), and execute it,
 * you'll see:
 * </p>
 *
 * <pre>
 * scala> (new StackFunSuite).execute()
 * StackFunSuite:
 * - empty is invoked on an empty stack
 * - peek is invoked on an empty stack
 * - pop is invoked on an empty stack
 * - empty is invoked on this non-empty stack: Stack(9)
 * - peek is invoked on this non-empty stack: Stack(9)
 * - pop is invoked on this non-empty stack: Stack(9)
 * - full is invoked on this non-full stack: Stack(9)
 * - push is invoked on this non-full stack: Stack(9)
 * - empty is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - peek is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - pop is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - full is invoked on this non-full stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - push is invoked on this non-full stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 * - full is invoked on a full stack
 * - empty is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 * - peek is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 * - pop is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 * - push is invoked on a full stack
 * </pre>
 * 
 * <p>
 * One thing to keep in mind when using shared tests is that in ScalaTest, each test in a suite must have a unique name.
 * If you register the same tests repeatedly in the same suite, one problem you may encounter is an exception at runtime
 * complaining that multiple tests are being registered with the same test name.
 * In a <code>FunSuite</code> there is no nesting construct analogous to <code>Spec</code>'s <code>describe</code> clause.
 * Therefore, you need to do a bit of
 * extra work to ensure that the test names are unique. If a duplicate test name problem shows up in a
 * <code>FunSuite</code>, you'll need to pass in a prefix or suffix string to add to each test name. You can pass this string
 * the same way you pass any other data needed by the shared tests, or just call <code>toString</code> on the shared fixture object.
 * This is the approach taken by the previous <code>FunSuiteStackBehaviors</code> example.
 * </p>
 *
 * <p>
 * Given this <code>FunSuiteStackBehaviors</code> trait, calling it with the <code>stackWithOneItem</code> fixture, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * </pre><pre class="stHighlighted">
 * testsFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * </pre>
 *
 * <p>
 * yields test names:
 * </p>
 *
 * <ul>
 * <li><code>empty is invoked on this non-empty stack: Stack(9)</code></li>
 * <li><code>peek is invoked on this non-empty stack: Stack(9)</code></li>
 * <li><code>pop is invoked on this non-empty stack: Stack(9)</code></li>
 * </ul>
 *
 * <p>
 * Whereas calling it with the <code>stackWithOneItemLessThanCapacity</code> fixture, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * testsFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
 * </pre><pre class="stHighlighted">
 * testsFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
 * </pre>
 *
 * <p>
 * yields different test names:
 * </p>
 *
 * <ul>
 * <li><code>empty is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)</code></li>
 * <li><code>peek is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)</code></li>
 * <li><code>pop is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)</code></li>
 * </ul>
 *
 * <h2>Tagging tests</h2>
 *
 * <p>
 * A <code>FunSuite</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing a <code>FunSuite</code>, groups of tests can
 * optionally be included and/or excluded. To tag a <code>FunSuite</code>'s tests,
 * you pass objects that extend abstract class <code>org.scalatest.Tag</code> to methods
 * that register tests, <code>test</code> and <code>ignore</code>. Class <code>Tag</code> takes one parameter, a string name.  If you have
 * created Java annotation interfaces for use as group names in direct subclasses of <code>org.scalatest.Suite</code>,
 * then you will probably want to use group names on your <code>FunSuite</code>s that match. To do so, simply 
 * pass the fully qualified names of the Java interfaces to the <code>Tag</code> constructor. For example, if you've
 * defined Java annotation interfaces with fully qualified names, <code>com.mycompany.groups.SlowTest</code> and
 * <code>com.mycompany.groups.DbTest</code>, then you could
 * create matching groups for <code>FunSuite</code>s like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.Tag
 *
 * object SlowTest extends Tag("com.mycompany.groups.SlowTest")
 * object DbTest extends Tag("com.mycompany.groups.DbTest")
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.Tag
 * <br /><span class="stReserved">object</span> <span class="stType">SlowTest</span> <span class="stReserved">extends</span> <span class="stType">Tag</span>(<span class="stQuotedString">"com.mycompany.groups.SlowTest"</span>)
 * <span class="stReserved">object</span> <span class="stType">DbTest</span> <span class="stReserved">extends</span> <span class="stType">Tag</span>(<span class="stQuotedString">"com.mycompany.groups.DbTest"</span>)
 * </pre>
 *
 * <p>
 * Given these definitions, you could place <code>FunSuite</code> tests into groups like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 *
 * class MySuite extends FunSuite {
 *
 *   test("addition", SlowTest) {
 *     val sum = 1 + 1
 *     assert(sum === 2)
 *     assert(sum + 2 === 4)
 *   }
 *
 *   test("subtraction", SlowTest, DbTest) {
 *     val diff = 4 - 1
 *     assert(diff === 3)
 *     assert(diff - 2 === 1)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> {
 * <br />  test(<span class="stQuotedString">"addition"</span>, <span class="stType">SlowTest</span>) {
 *     <span class="stReserved">val</span> sum = <span class="stLiteral">1</span> + <span class="stLiteral">1</span>
 *     assert(sum === <span class="stLiteral">2</span>)
 *     assert(sum + <span class="stLiteral">2</span> === <span class="stLiteral">4</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"subtraction"</span>, <span class="stType">SlowTest</span>, <span class="stType">DbTest</span>) {
 *     <span class="stReserved">val</span> diff = <span class="stLiteral">4</span> - <span class="stLiteral">1</span>
 *     assert(diff === <span class="stLiteral">3</span>)
 *     assert(diff - <span class="stLiteral">2</span> === <span class="stLiteral">1</span>)
 *   }
 * }
 * </pre>
 *
 * <p>
 * This code marks both tests, "addition" and "subtraction," with the <code>com.mycompany.groups.SlowTest</code> tag, 
 * and test "subtraction" with the <code>com.mycompany.groups.DbTest</code> tag.
 * </p>
 *
 * <p>
 * The primary <code>run</code> method takes a <code>Filter</code>, whose constructor takes an optional
 * <code>Set[String]</code>s called <code>tagsToInclude</code> and a <code>Set[String]</code> called
 * <code>tagsToExclude</code>. If <code>tagsToInclude</code> is <code>None</code>, all tests will be run
 * except those those belonging to tags listed in the
 * <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is defined, only tests
 * belonging to tags mentioned in the <code>tagsToInclude</code> set, and not mentioned in <code>tagsToExclude</code>,
 * will be run.
 * </p>
 *
 * <h2>Ignored tests</h2>
 *
 * <p>
 * To support the common use case of &#8220;temporarily&#8221; disabling a test, with the
 * good intention of resurrecting the test at a later time, <code>FunSuite</code> provides registration
 * methods that start with <code>ignore</code> instead of <code>test</code>. For example, to temporarily
 * disable the test named <code>addition</code>, just change &#8220;<code>test</code>&#8221; into &#8220;<code>ignore</code>,&#8221; like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 *
 * class MySuite extends FunSuite {
 *
 *   ignore("addition") {
 *     val sum = 1 + 1
 *     assert(sum === 2)
 *     assert(sum + 2 === 4)
 *   }
 *
 *   test("subtraction") {
 *     val diff = 4 - 1
 *     assert(diff === 3)
 *     assert(diff - 2 === 1)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> {
 * <br />  ignore(<span class="stQuotedString">"addition"</span>) {
 *     <span class="stReserved">val</span> sum = <span class="stLiteral">1</span> + <span class="stLiteral">1</span>
 *     assert(sum === <span class="stLiteral">2</span>)
 *     assert(sum + <span class="stLiteral">2</span> === <span class="stLiteral">4</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"subtraction"</span>) {
 *     <span class="stReserved">val</span> diff = <span class="stLiteral">4</span> - <span class="stLiteral">1</span>
 *     assert(diff === <span class="stLiteral">3</span>)
 *     assert(diff - <span class="stLiteral">2</span> === <span class="stLiteral">1</span>)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>MySuite</code> with:
 * </p>
 *
 * <pre>
 * scala> (new MySuite).execute()
 * </pre>
 *
 * <p>
 * It will run only <code>subtraction</code> and report that <code>addition</code> was ignored:
 * </p>
 *
 * <pre>
 * MySuite:
 * - addition !!! IGNORED !!!
 * - subtraction
 * </pre>
 *
 * <h2>Pending tests</h2>
 *
 * <p>
 * A <em>pending test</em> is one that has been given a name but is not yet implemented. The purpose of
 * pending tests is to facilitate a style of testing in which documentation of behavior is sketched
 * out before tests are written to verify that behavior (and often, the before the behavior of
 * the system being tested is itself implemented). Such sketches form a kind of specification of
 * what tests and functionality to implement later.
 * </p>
 *
 * <p>
 * To support this style of testing, a test can be given a name that specifies one
 * bit of behavior required by the system being tested. The test can also include some code that
 * sends more information about the behavior to the reporter when the tests run. At the end of the test,
 * it can call method <code>pending</code>, which will cause it to complete abruptly with <code>TestPendingException</code>.
 * Because tests in ScalaTest can be designated as pending with <code>TestPendingException</code>, both the test name and any information
 * sent to the reporter when running the test can appear in the report of a test run. (In other words,
 * the code of a pending test is executed just like any other test.) However, because the test completes abruptly
 * with <code>TestPendingException</code>, the test will be reported as pending, to indicate
 * the actual test, and possibly the functionality, has not yet been implemented.
 * </p>
 *
 * <p>
 * Although pending tests may be used more often in specification-style suites, such as
 * <code>org.scalatest.Spec</code>, you can also use it in <code>FunSuite</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 *
 * class MySuite extends FunSuite {
 *
 *   test("addition") {
 *     val sum = 1 + 1
 *     assert(sum === 2)
 *     assert(sum + 2 === 4)
 *   }
 *
 *   test("subtraction") (pending)
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> {
 * <br />  test(<span class="stQuotedString">"addition"</span>) {
 *     <span class="stReserved">val</span> sum = <span class="stLiteral">1</span> + <span class="stLiteral">1</span>
 *     assert(sum === <span class="stLiteral">2</span>)
 *     assert(sum + <span class="stLiteral">2</span> === <span class="stLiteral">4</span>)
 *   }
 * <br />  test(<span class="stQuotedString">"subtraction"</span>) (pending)
 * }
 * </pre>
 *
 * <p>
 * (Note: "<code>(pending)</code>" is the body of the test. Thus the test contains just one statement, an invocation
 * of the <code>pending</code> method, which throws <code>TestPendingException</code>.)
 * If you run this version of <code>MySuite</code> with:
 * </p>
 *
 * <pre>
 * scala> (new MySuite).execute()
 * </pre>
 *
 * <p>
 * It will run both tests, but report that <code>subtraction</code> is pending. You'll see:
 * </p>
 *
 * <pre>
 * MySuite:
 * - addition
 * - subtraction (pending)
 * </pre>
 * 
 * <h2>Informers</h2>
 *
 * <p>
 * One of the parameters to the primary <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default by <code>FunSuite</code>'s methods will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <code>Informer</code> that will forward information to the current <code>Reporter</code>
 * is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via one of its <code>apply</code> methods.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <code>InfoProvided</code> event.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 *
 * class MySuite extends FunSuite {
 *
 *   test("addition") {
 *     val sum = 1 + 1
 *     assert(sum === 2)
 *     assert(sum + 2 === 4)
 *     info("Addition seems to work")
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FunSuite
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FunSuite</span> {
 * <br />  test(<span class="stQuotedString">"addition"</span>) {
 *     <span class="stReserved">val</span> sum = <span class="stLiteral">1</span> + <span class="stLiteral">1</span>
 *     assert(sum === <span class="stLiteral">2</span>)
 *     assert(sum + <span class="stLiteral">2</span> === <span class="stLiteral">4</span>)
 *     info(<span class="stQuotedString">"Addition seems to work"</span>)
 *   }
 * }
 * </pre>
 *
 * If you run this <code>Suite</code> from the interpreter, you will see the following message
 * included in the printed report:
 *
 * <pre>
 * MySuite:
 * - addition
 *   + Addition seems to work 
 * </pre>
 *
 * @author Bill Venners
 */
trait FunSuite extends Suite { thisSuite =>

  private final val engine = new Engine("concurrentFunSuiteMod", "FunSuite")
  import engine._

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>FunSuite</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def info: Informer = atomicInformer.get

  /**
   * Register a test with the specified name, optional tags, and function value that takes no arguments.
   * This method will register the test for later execution via an invocation of one of the <code>run</code>
   * methods. The passed test name must not have been registered previously on
   * this <code>FunSuite</code> instance.
   *
   * @param testName the name of the test
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws NotAllowedException if <code>testName</code> had been registered previously
   * @throws NullPointerException if <code>testName</code> or any passed test tag is <code>null</code>
   */
  protected def test(testName: String, testTags: Tag*)(testFun: => Unit) {
    registerTest(testName, testFun _, "testCannotAppearInsideAnotherTest", "FunSuite.scala", "test", testTags: _*)
  }

  /**
   * Register a test to ignore, which has the specified name, optional tags, and function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>run</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>test</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be run, but a
   * report will be sent that indicates the test was ignored. The passed test name must not have been registered previously on
   * this <code>FunSuite</code> instance.
   *
   * @param testName the name of the test
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws NotAllowedException if <code>testName</code> had been registered previously
   */
  protected def ignore(testName: String, testTags: Tag*)(testFun: => Unit) {
    registerIgnoredTest(testName, testFun _, "ignoreCannotAppearInsideATest", "FunSuite.scala", "ignore", testTags: _*)
  }

  /**
  * An immutable <code>Set</code> of test names. If this <code>FunSuite</code> contains no tests, this method returns an empty <code>Set</code>.
  *
  * <p>
  * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's iterator will
  * return those names in the order in which the tests were registered.
  * </p>
  */
  override def testNames: Set[String] = {
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by <code>testName</code>.
   *
   * @param testName the name of one test to run.
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param configMap a <code>Map</code> of properties that can be used by the executing <code>Suite</code> of tests.
   * @throws IllegalArgumentException if <code>testName</code> is defined but a test with that name does not exist on this <code>FunSuite</code>
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, reporter: Reporter, stopper: Stopper, configMap: Map[String, Any], tracker: Tracker) {

    def invokeWithFixture(theTest: TestLeaf) {
      val theConfigMap = configMap
      withFixture(
        new NoArgTest {
          def name = testName
          def apply() { theTest.testFun() }
          def configMap = theConfigMap
        }
      )
    }

    runTestImpl(thisSuite, testName, reporter, stopper, configMap, tracker, true, invokeWithFixture)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>FunSuite</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>FunSuite</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to 
   * methods <code>test</code> and <code>ignore</code>. 
   * </p>
   */
  override def tags: Map[String, Set[String]] = atomic.get.tagsMap

  /**
   * Run zero to many of this <code>Spec</code>'s tests.
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param filter a <code>Filter</code> with which to filter tests based on their tags
   * @param configMap a <code>Map</code> of key-value pairs that can be used by the executing <code>Suite</code> of tests.
   * @param distributor an optional <code>Distributor</code>, into which to put nested <code>Suite</code>s to be run
   *              by another entity, such as concurrently by a pool of threads. If <code>None</code>, nested <code>Suite</code>s will be run sequentially.
   * @param tracker a <code>Tracker</code> tracking <code>Ordinal</code>s being fired by the current thread.
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected override def runTests(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {

    runTestsImpl(thisSuite, testName, reporter, stopper, filter, configMap, distributor, tracker, info, true, runTest)
  }

  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {

    runImpl(thisSuite, testName, reporter, stopper, filter, configMap, distributor, tracker, super.run)
  }

  /**
   * Registers shared tests.
   *
   * <p>
   * This method enables the following syntax for shared tests in a <code>FunSuite</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * testsFor(nonEmptyStack(lastValuePushed))
   * </pre><pre class="stHighlighted">
   * testsFor(nonEmptyStack(lastValuePushed))
   * </pre>
   *
   * <p>
   * This method just provides syntax sugar intended to make the intent of the code clearer.
   * Because the parameter passed to it is
   * type <code>Unit</code>, the expression will be evaluated before being passed, which
   * is sufficient to register the shared tests. For examples of shared tests, see the
   * <a href="#SharedTests">Shared tests section</a> in the main documentation for this trait.
   * </p>
   */
  protected def testsFor(unit: Unit) {}
}

private[scalatest] object FunSuite {
  val IgnoreTagName = "org.scalatest.Ignore"
}