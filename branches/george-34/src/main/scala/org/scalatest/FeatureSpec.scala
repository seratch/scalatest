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

import NodeFamily._
import scala.collection.immutable.ListSet
import org.scalatest.StackDepthExceptionHelper.getStackDepth
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import Suite.anErrorThatShouldCauseAnAbort

/**
 * A suite of tests in which each test represents one <em>scenario</em> of a <em>feature</em>. 
 * <code>FeatureSpec</code> is intended for writing tests that are "higher level" than unit tests, for example, integration
 * tests, functional tests, and acceptance tests. You can use <code>FeatureSpec</code> for unit testing if you prefer, however.
 * Here's an example:
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import org.scalatest.GivenWhenThen
 * import scala.collection.mutable.Stack
 * 
 * class StackFeatureSpec extends FeatureSpec with GivenWhenThen {
 * 
 *   feature("The user can pop an element off the top of the stack") {
 * 
 *     info("As a programmer")
 *     info("I want to be able to pop items off the stack")
 *     info("So that I can get them in last-in-first-out order")
 * 
 *     scenario("pop is invoked on a non-empty stack") {
 * 
 *       given("a non-empty stack")
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       val oldSize = stack.size
 * 
 *       when("when pop is invoked on the stack")
 *       val result = stack.pop()
 * 
 *       then("the most recently pushed element should be returned")
 *       assert(result === 2)
 * 
 *       and("the stack should have one less item than before")
 *       assert(stack.size === oldSize - 1)
 *     }
 * 
 *     scenario("pop is invoked on an empty stack") {
 * 
 *       given("an empty stack")
 *       val emptyStack = new Stack[String]
 * 
 *       when("when pop is invoked on the stack")
 *       then("NoSuchElementException should be thrown")
 *       intercept[NoSuchElementException] {
 *         emptyStack.pop()
 *       }
 * 
 *       and("the stack should still be empty")
 *       assert(emptyStack.isEmpty)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <span class="stReserved">import</span> org.scalatest.GivenWhenThen
 * <span class="stReserved">import</span> scala.collection.mutable.Stack
 * <br /><span class="stReserved">class</span> <span class="stType">StackFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> <span class="stReserved">with</span> <span class="stType">GivenWhenThen</span> {
 * <br />  feature(<span class="stQuotedString">"The user can pop an element off the top of the stack"</span>) {
 * <br />    info(<span class="stQuotedString">"As a programmer"</span>)
 *     info(<span class="stQuotedString">"I want to be able to pop items off the stack"</span>)
 *     info(<span class="stQuotedString">"So that I can get them in last-in-first-out order"</span>)
 * <br />    scenario(<span class="stQuotedString">"pop is invoked on a non-empty stack"</span>) {
 * <br />      given(<span class="stQuotedString">"a non-empty stack"</span>)
 *       <span class="stReserved">val</span> stack = <span class="stReserved">new</span> <span class="stType">Stack[Int]</span>
 *       stack.push(<span class="stLiteral">1</span>)
 *       stack.push(<span class="stLiteral">2</span>)
 *       <span class="stReserved">val</span> oldSize = stack.size
 * <br />      when(<span class="stQuotedString">"when pop is invoked on the stack"</span>)
 *       <span class="stReserved">val</span> result = stack.pop()
 * <br />      then(<span class="stQuotedString">"the most recently pushed element should be returned"</span>)
 *       assert(result === <span class="stLiteral">2</span>)
 * <br />      and(<span class="stQuotedString">"the stack should have one less item than before"</span>)
 *       assert(stack.size === oldSize - <span class="stLiteral">1</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"pop is invoked on an empty stack"</span>) {
 * <br />      given(<span class="stQuotedString">"an empty stack"</span>)
 *       <span class="stReserved">val</span> emptyStack = <span class="stReserved">new</span> <span class="stType">Stack[String]</span>
 * <br />      when(<span class="stQuotedString">"when pop is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"NoSuchElementException should be thrown"</span>)
 *       intercept[<span class="stType">NoSuchElementException</span>] {
 *         emptyStack.pop()
 *       }
 * <br />      and(<span class="stQuotedString">"the stack should still be empty"</span>)
 *       assert(emptyStack.isEmpty)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * A <code>FeatureSpec</code> contains <em>feature clauses</em> and <em>scenarios</em>. You define a feature clause
 * with <code>feature</code>, and a scenario with <code>scenario</code>. Both
 * <code>feature</code> and <code>scenario</code> are methods, defined in
 * <code>FeatureSpec</code>, which will be invoked
 * by the primary constructor of <code>StackFeatureSpec</code>. 
 * A feature clause describes a feature of the <em>subject</em> (class or other entity) you are specifying
 * and testing. In the previous example, 
 * the subject under specification and test is a stack. The feature being specified and tested is 
 * the ability for a user (a programmer in this case) to pop an element off the top of the stack. With each scenario you provide a
 * string (the <em>spec text</em>) that specifies the behavior of the subject for
 * one scenario in which the feature may be used, and a block of code that tests that behavior.
 * You place the spec text between the parentheses, followed by the test code between curly
 * braces.  The test code will be wrapped up as a function passed as a by-name parameter to
 * <code>scenario</code>, which will register the test for later execution.
 * </p>
 *
 * <p>
 * A <code>FeatureSpec</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Scenarios can only be registered with the <code>scenario</code> method while the <code>FeatureSpec</code> is
 * in its registration phase. Any attempt to register a scenario after the <code>FeatureSpec</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>FeatureSpec</code>,
 * will be met with a thrown <code>TestRegistrationClosedException</code>. The recommended style
 * of using <code>FeatureSpec</code> is to register tests during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <p>
 * Each scenario represents one test. The name of the test is the spec text passed to the <code>scenario</code> method.
 * The feature name does not appear as part of the test name. In a <code>FeatureSpec</code>, therefore, you must take care
 * to ensure that each test has a unique name (in other words, that each <code>scenario</code> has unique spec text).
 * </p>
 *
 * <p>
 * When you run a <code>FeatureSpec</code>, it will send <code>Formatter</code>s in the events it sends to the
 * <code>Reporter</code>. ScalaTest's built-in reporters will report these events in such a way
 * that the output is easy to read as an informal specification of the <em>subject</em> being tested.
 * For example, if you ran <code>StackFeatureSpec</code> from within the Scala interpreter:
 * </p>
 *
 * <pre>
 * scala> (new StackFeatureSpec).execute()
 * </pre>
 *
 * <p>
 * You would see:
 * </p>
 *
 * <pre>
 * Feature: The user can pop an element off the top of the stack 
 *   As a programmer 
 *   I want to be able to pop items off the stack 
 *   So that I can get them in last-in-first-out order 
 *   Scenario: pop is invoked on a non-empty stack
 *     Given a non-empty stack 
 *     When when pop is invoked on the stack 
 *     Then the most recently pushed element should be returned 
 *     And the stack should have one less item than before 
 *   Scenario: pop is invoked on an empty stack
 *     Given an empty stack 
 *     When when pop is invoked on the stack 
 *     Then NoSuchElementException should be thrown 
 *     And the stack should still be empty 
 * </pre>
 *
 * <p>
 * See also: <a href="http://www.scalatest.org/getting_started_with_feature_spec" target="_blank">Getting started with <code>FeatureSpec</code>.</a>
 * </p>
 * 
 * <h2>Shared fixtures</h2>
 *
 * <p>
 * A test <em>fixture</em> is objects or other artifacts (such as files, sockets, database
 * connections, etc.) used by tests to do their work. You can use fixtures in
 * <code>FeatureSpec</code>s with the same approaches suggested for <code>Suite</code> in
 * its documentation. The same text that appears in the test fixture
 * section of <code>Suite</code>'s documentation is repeated here, with examples changed from
 * <code>Suite</code> to <code>FeatureSpec</code>.
 * </p>
 *
 * <p>
 * If a fixture is used by only one test, then the definitions of the fixture objects can
 * be local to the test function, such as the objects assigned to <code>stack</code> and <code>emptyStack</code> in the
 * previous <code>StackFeatureSpec</code> examples. If multiple tests need to share a fixture, the best approach
 * is to assign them to instance variables. Here's a (very contrived) example, in which the object assigned
 * to <code>shared</code> is used by multiple test functions:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 *
 * class ArithmeticFeatureSpec extends FeatureSpec {
 *
 *   // Sharing immutable fixture objects via instance variables
 *   val shared = 5
 *
 *   feature("Integer arithmetic") {
 *
 *     scenario("addition") {
 *       val sum = 2 + 3
 *       assert(sum === shared)
 *     }
 *
 *     scenario("subtraction") {
 *       val diff = 7 - 2
 *       assert(diff === shared)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <br /><span class="stReserved">class</span> <span class="stType">ArithmeticFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> {
 * <br />  <span class="stLineComment">// Sharing immutable fixture objects via instance variables</span>
 *   <span class="stReserved">val</span> shared = <span class="stLiteral">5</span>
 * <br />  feature(<span class="stQuotedString">"Integer arithmetic"</span>) {
 * <br />    scenario(<span class="stQuotedString">"addition"</span>) {
 *       <span class="stReserved">val</span> sum = <span class="stLiteral">2</span> + <span class="stLiteral">3</span>
 *       assert(sum === shared)
 *     }
 * <br />    scenario(<span class="stQuotedString">"subtraction"</span>) {
 *       <span class="stReserved">val</span> diff = <span class="stLiteral">7</span> - <span class="stLiteral">2</span>
 *       assert(diff === shared)
 *     }
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
 * import org.scalatest.FeatureSpec
 * import scala.collection.mutable.ListBuffer
 *
 * class MyFeatureSpec extends FeatureSpec {
 *
 *   // create objects needed by tests and return as a tuple
 *   def createFixture = (
 *     new StringBuilder("ScalaTest is "),
 *     new ListBuffer[String]
 *   )
 *
 *   feature("The create-fixture approach") {
 *
 *     scenario("shared fixture objects are mutated by a test") {
 *       val (builder, lbuf) = createFixture
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(lbuf.isEmpty)
 *       lbuf += "sweet"
 *     }
 *
 *     scenario("test gets a fresh copy of the shared fixture") {
 *       val (builder, lbuf) = createFixture
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(lbuf.isEmpty)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <span class="stReserved">import</span> scala.collection.mutable.ListBuffer
 * <br /><span class="stReserved">class</span> <span class="stType">MyFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> {
 * <br />  <span class="stLineComment">// create objects needed by tests and return as a tuple</span>
 *   <span class="stReserved">def</span> createFixture = (
 *     <span class="stReserved">new</span> <span class="stType">StringBuilder</span>(<span class="stQuotedString">"ScalaTest is "</span>),
 *     <span class="stReserved">new</span> <span class="stType">ListBuffer[String]</span>
 *   )
 * <br />  feature(<span class="stQuotedString">"The create-fixture approach"</span>) {
 * <br />    scenario(<span class="stQuotedString">"shared fixture objects are mutated by a test"</span>) {
 *       <span class="stReserved">val</span> (builder, lbuf) = createFixture
 *       builder.append(<span class="stQuotedString">"easy!"</span>)
 *       assert(builder.toString === <span class="stQuotedString">"ScalaTest is easy!"</span>)
 *       assert(lbuf.isEmpty)
 *       lbuf += <span class="stQuotedString">"sweet"</span>
 *     }
 * <br />    scenario(<span class="stQuotedString">"test gets a fresh copy of the shared fixture"</span>) {
 *       <span class="stReserved">val</span> (builder, lbuf) = createFixture
 *       builder.append(<span class="stQuotedString">"fun!"</span>)
 *       assert(builder.toString === <span class="stQuotedString">"ScalaTest is fun!"</span>)
 *       assert(lbuf.isEmpty)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If different tests in the same <code>FeatureSpec</code> require different fixtures, you can create multiple create-fixture methods and
 * call the method (or methods) needed by each test at the begining of the test. If every test requires the same set of
 * mutable fixture objects, one other approach you can take is make them simply <code>val</code>s and mix in trait
 * <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>.  If you mix in <code>OneInstancePerTest</code>, each test
 * will be run in its own instance of the <code>FeatureSpec</code>, similar to the way JUnit tests are executed.
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
 * import org.scalatest.FeatureSpec
 * import org.scalatest.BeforeAndAfterEach
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class FileIoFeatureSpec extends FeatureSpec with BeforeAndAfterEach {
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
 *   feature("Reading and writing files") {
 *
 *     scenario("reading from a temp file") {
 *       var builder = new StringBuilder
 *       var c = reader.read()
 *       while (c != -1) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === "Hello, test!")
 *     }
 *
 *     scenario("reading first char of a temp file") {
 *       assert(reader.read() === 'H')
 *     }
 * 
 *     scenario("no fixture is passed") { 
 *       assert(1 + 1 === 2)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <span class="stReserved">import</span> org.scalatest.BeforeAndAfterEach
 * <span class="stReserved">import</span> java.io.FileReader
 * <span class="stReserved">import</span> java.io.FileWriter
 * <span class="stReserved">import</span> java.io.File
 * <br /><span class="stReserved">class</span> <span class="stType">FileIoFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> <span class="stReserved">with</span> <span class="stType">BeforeAndAfterEach</span> {
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
 * <br />  feature(<span class="stQuotedString">"Reading and writing files"</span>) {
 * <br />    scenario(<span class="stQuotedString">"reading from a temp file"</span>) {
 *       <span class="stReserved">var</span> builder = <span class="stReserved">new</span> <span class="stType">StringBuilder</span>
 *       <span class="stReserved">var</span> c = reader.read()
 *       <span class="stReserved">while</span> (c != -<span class="stLiteral">1</span>) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === <span class="stQuotedString">"Hello, test!"</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"reading first char of a temp file"</span>) {
 *       assert(reader.read() === <span class="stQuotedString">'H'</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"no fixture is passed"</span>) { 
 *       assert(<span class="stLiteral">1</span> + <span class="stLiteral">1</span> === <span class="stLiteral">2</span>)
 *     }
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
 * import org.scalatest.FeatureSpec
 * import org.scalatest.BeforeAndAfterEach
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 *
 * class FileIoFeatureSpec extends FeatureSpec {
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
 *   feature("Reading and writing files") {
 *
 *     scenario("reading from a temp file") {
 *       var builder = new StringBuilder
 *       var c = reader.read()
 *       while (c != -1) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === "Hello, test!")
 *     }
 *
 *     scenario("reading first char of a temp file") {
 *       assert(reader.read() === 'H')
 *     }
 * 
 *     scenario("no fixture is passed") { 
 *       assert(1 + 1 === 2)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <span class="stReserved">import</span> org.scalatest.BeforeAndAfterEach
 * <span class="stReserved">import</span> java.io.FileReader
 * <span class="stReserved">import</span> java.io.FileWriter
 * <span class="stReserved">import</span> java.io.File
 * <br /><span class="stReserved">class</span> <span class="stType">FileIoFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> {
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
 * <br />  feature(<span class="stQuotedString">"Reading and writing files"</span>) {
 * <br />    scenario(<span class="stQuotedString">"reading from a temp file"</span>) {
 *       <span class="stReserved">var</span> builder = <span class="stReserved">new</span> <span class="stType">StringBuilder</span>
 *       <span class="stReserved">var</span> c = reader.read()
 *       <span class="stReserved">while</span> (c != -<span class="stLiteral">1</span>) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === <span class="stQuotedString">"Hello, test!"</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"reading first char of a temp file"</span>) {
 *       assert(reader.read() === <span class="stQuotedString">'H'</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"no fixture is passed"</span>) { 
 *       assert(<span class="stLiteral">1</span> + <span class="stLiteral">1</span> === <span class="stLiteral">2</span>)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you prefer to keep your test classes immutable, one final variation is to use the
 * <a href="fixture/FixtureFeatureSpec.html"><code>FixtureFeatureSpec</code></a> trait from the
 * <code>org.scalatest.fixture</code> package.  Tests in an <code>org.scalatest.fixture.FixtureFeatureSpec</code> can have a fixture
 * object passed in as a parameter. You must indicate the type of the fixture object
 * by defining the <code>Fixture</code> type member and define a <code>withFixture</code> method that takes a <em>one-arg</em> test function.
 * (A <code>FixtureFeatureSpec</code> has two overloaded <code>withFixture</code> methods, therefore, one that takes a <code>OneArgTest</code>
 * and the other, inherited from <code>Suite</code>, that takes a <code>NoArgTest</code>.)
 * Inside the <code>withFixture(OneArgTest)</code> method, you create the fixture, pass it into the test function, then perform any
 * necessary cleanup after the test function returns. Instead of invoking each test directly, a <code>FixtureFeatureSpec</code> will
 * pass a function that invokes the code of a test to <code>withFixture(OneArgTest)</code>. Your <code>withFixture(OneArgTest)</code> method, therefore,
 * is responsible for actually running the code of the test by invoking the test function.
 * For example, you could pass the temp file reader fixture to each test that needs it
 * by overriding the <code>withFixture(OneArgTest)</code> method of a <code>FixtureFeatureSpec</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture.FixtureFeatureSpec
 * import java.io.FileReader
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class MySuite extends FixtureFeatureSpec {
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
 *   feature("Reading and writing files") {
 *
 *     scenario("reading from a temp file") { reader =>
 *       var builder = new StringBuilder
 *       var c = reader.read()
 *       while (c != -1) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === "Hello, test!")
 *     }
 * 
 *     scenario("reading first char of a temp file") { reader =>
 *       assert(reader.read() === 'H')
 *     }
 * 
 *     scenario("no fixture is passed") { () =>
 *       assert(1 + 1 === 2)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.fixture.FixtureFeatureSpec
 * <span class="stReserved">import</span> java.io.FileReader
 * <span class="stReserved">import</span> java.io.FileWriter
 * <span class="stReserved">import</span> java.io.File
 * <br /><span class="stReserved">class</span> <span class="stType">MySuite</span> <span class="stReserved">extends</span> <span class="stType">FixtureFeatureSpec</span> {
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
 * <br />  feature(<span class="stQuotedString">"Reading and writing files"</span>) {
 * <br />    scenario(<span class="stQuotedString">"reading from a temp file"</span>) { reader =>
 *       <span class="stReserved">var</span> builder = <span class="stReserved">new</span> <span class="stType">StringBuilder</span>
 *       <span class="stReserved">var</span> c = reader.read()
 *       <span class="stReserved">while</span> (c != -<span class="stLiteral">1</span>) {
 *         builder.append(c.toChar)
 *         c = reader.read()
 *       }
 *       assert(builder.toString === <span class="stQuotedString">"Hello, test!"</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"reading first char of a temp file"</span>) { reader =>
 *       assert(reader.read() === <span class="stQuotedString">'H'</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"no fixture is passed"</span>) { () =>
 *       assert(<span class="stLiteral">1</span> + <span class="stLiteral">1</span> === <span class="stLiteral">2</span>)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * It is worth noting that the only difference in the test code between the mutable
 * <code>BeforeAndAfterEach</code> approach shown here and the immutable <code>FixtureFeatureSpec</code>
 * approach shown previously is that two of the <code>FixtureFeatureSpec</code>'s test functions take a <code>FileReader</code> as
 * a parameter via the "<code>reader =></code>" at the beginning of the function. Otherwise the test code is identical.
 * One benefit of the explicit parameter is that, as demonstrated
 * by the "<code>no fixture passed</code>" scenario, a <code>FixtureFeatureSpec</code>
 * test need not take the fixture. So you can have some tests that take a fixture, and others that don't.
 * In this case, the <code>FixtureFeatureSpec</code> provides documentation indicating which
 * tests use the fixture and which don't, whereas the <code>BeforeAndAfterEach</code> approach does not.
 * (If you have want to combine tests that take different fixture types in the same <code>FeatureSpec</code>, you can
 * use <a href="fixture/MultipleFixtureFeatureSpec.html">MultipleFixtureFeatureSpec</a>.)
 * </p>
 *
 * <p>
 * If you want to execute code before and after all tests (and nested suites) in a suite, such
 * as you could do with <code>@BeforeClass</code> and <code>@AfterClass</code>
 * annotations in JUnit 4, you can use the <code>beforeAll</code> and <code>afterAll</code>
 * methods of <code>BeforeAndAfterAll</code>. See the documentation for <code>BeforeAndAfterAll</code> for
 * an example.
 * </p>
 *
 * <a name="SharedScenarios"></a><h2>Shared scenarios</h2>
 *
 * <p>
 * Sometimes you may want to run the same test code on different fixture objects. In other words, you may want to write tests that are "shared"
 * by different fixture objects.
 * To accomplish this in a <code>FeatureSpec</code>, you first place shared tests (<em>i.e.</em>, shared scenarios) in
 * <em>behavior functions</em>. These behavior functions will be
 * invoked during the construction phase of any <code>FeatureSpec</code> that uses them, so that the scenarios they contain will
 * be registered as scenarios in that <code>FeatureSpec</code>.
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
 * <em>etc</em>. You may find you have several scenarios that make sense any time the stack is non-empty. Thus you'd ideally want to run
 * those same scenarios for three stack fixture objects: a full stack, a stack with a one item, and a stack with one item less than
 * capacity. With shared tests, you can factor these scenarios out into a behavior function, into which you pass the
 * stack fixture to use when running the tests. So in your <code>FeatureSpec</code> for stack, you'd invoke the
 * behavior function three times, passing in each of the three stack fixtures so that the shared scenarios are run for all three fixtures.
 * </p>
 *
 * <p>
 * You can define a behavior function that encapsulates these shared scenarios inside the <code>FeatureSpec</code> that uses them. If they are shared
 * between different <code>FeatureSpec</code>s, however, you could also define them in a separate trait that is mixed into
 * each <code>FeatureSpec</code> that uses them.
 * <a name="StackBehaviors">For</a> example, here the <code>nonEmptyStack</code> behavior function (in this case, a
 * behavior <em>method</em>) is defined in a trait along with another
 * method containing shared scenarios for non-full stacks:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import org.scalatest.GivenWhenThen
 * import org.scalatestexamples.helpers.Stack
 * 
 * trait FeatureSpecStackBehaviors { this: FeatureSpec with GivenWhenThen =>
 * 
 *   def nonEmptyStack(createNonEmptyStack: => Stack[Int], lastItemAdded: Int) {
 * 
 *     scenario("empty is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
 * 
 *       given("a non-empty stack")
 *       val stack = createNonEmptyStack
 * 
 *       when("empty is invoked on the stack")
 *       then("empty returns false")
 *       assert(!stack.empty)
 *     }
 * 
 *     scenario("peek is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
 * 
 *       given("a non-empty stack")
 *       val stack = createNonEmptyStack
 *       val size = stack.size
 * 
 *       when("peek is invoked on the stack")
 *       then("peek returns the last item added")
 *       assert(stack.peek === lastItemAdded)
 * 
 *       and("the size of the stack is the same as before")
 *       assert(stack.size === size)
 *     }
 * 
 *     scenario("pop is invoked on this non-empty stack: " + createNonEmptyStack.toString) {
 * 
 *       given("a non-empty stack")
 *       val stack = createNonEmptyStack
 *       val size = stack.size
 * 
 *       when("pop is invoked on the stack")
 *       then("pop returns the last item added")
 *       assert(stack.pop === lastItemAdded)
 * 
 *       and("the size of the stack one less than before")
 *       assert(stack.size === size - 1)
 *     }
 *   }
 *   
 *   def nonFullStack(createNonFullStack: => Stack[Int]) {
 *       
 *     scenario("full is invoked on this non-full stack: " + createNonFullStack.toString) {
 * 
 *       given("a non-full stack")
 *       val stack = createNonFullStack
 * 
 *       when("full is invoked on the stack")
 *       then("full returns false")
 *       assert(!stack.full)
 *     }
 *       
 *     scenario("push is invoked on this non-full stack: " + createNonFullStack.toString) {
 * 
 *       given("a non-full stack")
 *       val stack = createNonFullStack
 *       val size = stack.size
 * 
 *       when("push is invoked on the stack")
 *       stack.push(7)
 * 
 *       then("the size of the stack is one greater than before")
 *       assert(stack.size === size + 1)
 * 
 *       and("the top of the stack contains the pushed value")
 *       assert(stack.peek === 7)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <span class="stReserved">import</span> org.scalatest.GivenWhenThen
 * <span class="stReserved">import</span> org.scalatestexamples.helpers.Stack
 * <br /><span class="stReserved">trait</span> <span class="stType">FeatureSpecStackBehaviors</span> { <span class="stReserved">this</span>: <span class="stType">FeatureSpec</span> <span class="stReserved">with</span> <span class="stType">GivenWhenThen</span> =>
 * <br />  <span class="stReserved">def</span> nonEmptyStack(createNonEmptyStack: => <span class="stType">Stack[Int]</span>, lastItemAdded: <span class="stType">Int</span>) {
 * <br />    scenario(<span class="stQuotedString">"empty is invoked on this non-empty stack: "</span> + createNonEmptyStack.toString) {
 * <br />      given(<span class="stQuotedString">"a non-empty stack"</span>)
 *       <span class="stReserved">val</span> stack = createNonEmptyStack
 * <br />      when(<span class="stQuotedString">"empty is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"empty returns false"</span>)
 *       assert(!stack.empty)
 *     }
 * <br />    scenario(<span class="stQuotedString">"peek is invoked on this non-empty stack: "</span> + createNonEmptyStack.toString) {
 * <br />      given(<span class="stQuotedString">"a non-empty stack"</span>)
 *       <span class="stReserved">val</span> stack = createNonEmptyStack
 *       <span class="stReserved">val</span> size = stack.size
 * <br />      when(<span class="stQuotedString">"peek is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"peek returns the last item added"</span>)
 *       assert(stack.peek === lastItemAdded)
 * <br />      and(<span class="stQuotedString">"the size of the stack is the same as before"</span>)
 *       assert(stack.size === size)
 *     }
 * <br />    scenario(<span class="stQuotedString">"pop is invoked on this non-empty stack: "</span> + createNonEmptyStack.toString) {
 * <br />      given(<span class="stQuotedString">"a non-empty stack"</span>)
 *       <span class="stReserved">val</span> stack = createNonEmptyStack
 *       <span class="stReserved">val</span> size = stack.size
 * <br />      when(<span class="stQuotedString">"pop is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"pop returns the last item added"</span>)
 *       assert(stack.pop === lastItemAdded)
 * <br />      and(<span class="stQuotedString">"the size of the stack one less than before"</span>)
 *       assert(stack.size === size - <span class="stLiteral">1</span>)
 *     }
 *   }
 * <br />  <span class="stReserved">def</span> nonFullStack(createNonFullStack: => <span class="stType">Stack[Int]</span>) {
 * <br />    scenario(<span class="stQuotedString">"full is invoked on this non-full stack: "</span> + createNonFullStack.toString) {
 * <br />      given(<span class="stQuotedString">"a non-full stack"</span>)
 *       <span class="stReserved">val</span> stack = createNonFullStack
 * <br />      when(<span class="stQuotedString">"full is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"full returns false"</span>)
 *       assert(!stack.full)
 *     }
 * <br />    scenario(<span class="stQuotedString">"push is invoked on this non-full stack: "</span> + createNonFullStack.toString) {
 * <br />      given(<span class="stQuotedString">"a non-full stack"</span>)
 *       <span class="stReserved">val</span> stack = createNonFullStack
 *       <span class="stReserved">val</span> size = stack.size
 * <br />      when(<span class="stQuotedString">"push is invoked on the stack"</span>)
 *       stack.push(<span class="stLiteral">7</span>)
 * <br />      then(<span class="stQuotedString">"the size of the stack is one greater than before"</span>)
 *       assert(stack.size === size + <span class="stLiteral">1</span>)
 * <br />      and(<span class="stQuotedString">"the top of the stack contains the pushed value"</span>)
 *       assert(stack.peek === <span class="stLiteral">7</span>)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Given these behavior functions, you could invoke them directly, but <code>FeatureSpec</code> offers a DSL for the purpose,
 * which looks like this:
 * </p>
 *
 * <pre class="stHighlight">
 * scenariosFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * scenariosFor(nonFullStack(stackWithOneItem))
 * </pre><pre class="stHighlighted">
 * scenariosFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * scenariosFor(nonFullStack(stackWithOneItem))
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
 * scenariosFor(nonEmptyStack) // assuming lastValuePushed is also in scope inside nonEmptyStack
 * scenariosFor(nonFullStack)
 * </pre><pre class="stHighlighted">
 * scenariosFor(nonEmptyStack) <span class="stLineComment">// assuming lastValuePushed is also in scope inside nonEmptyStack</span>
 * scenariosFor(nonFullStack)
 * </pre>
 *
 * <p>
 * The recommended style, however, is the functional, pass-all-the-needed-values-in style. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import org.scalatest.GivenWhenThen
 * import org.scalatestexamples.helpers.Stack
 * 
 * class StackFeatureSpec extends FeatureSpec with GivenWhenThen with FeatureSpecStackBehaviors {
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
 *   feature("A Stack is pushed and popped") {
 *  
 *     scenario("empty is invoked on an empty stack") {
 * 
 *       given("an empty stack")
 *       val stack = emptyStack
 * 
 *       when("empty is invoked on the stack")
 *       then("empty returns true")
 *       assert(stack.empty)
 *     }
 *  
 *     scenario("peek is invoked on an empty stack") {
 * 
 *       given("an empty stack")
 *       val stack = emptyStack
 * 
 *       when("peek is invoked on the stack")
 *       then("peek throws IllegalStateException")
 *       intercept[IllegalStateException] {
 *         stack.peek
 *       }
 *     }
 *  
 *     scenario("pop is invoked on an empty stack") {
 * 
 *       given("an empty stack")
 *       val stack = emptyStack
 * 
 *       when("pop is invoked on the stack")
 *       then("pop throws IllegalStateException")
 *       intercept[IllegalStateException] {
 *         emptyStack.pop
 *       }
 *     }
 *  
 *     scenariosFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 *     scenariosFor(nonFullStack(stackWithOneItem))
 *  
 *     scenariosFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
 *     scenariosFor(nonFullStack(stackWithOneItemLessThanCapacity))
 *  
 *     scenario("full is invoked on a full stack") {
 * 
 *       given("an full stack")
 *       val stack = fullStack
 * 
 *       when("full is invoked on the stack")
 *       then("full returns true")
 *       assert(stack.full)
 *     }
 *  
 *     scenariosFor(nonEmptyStack(fullStack, lastValuePushed))
 *  
 *     scenario("push is invoked on a full stack") {
 * 
 *       given("an full stack")
 *       val stack = fullStack
 * 
 *       when("push is invoked on the stack")
 *       then("push throws IllegalStateException")
 *       intercept[IllegalStateException] {
 *         stack.push(10)
 *       }
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <span class="stReserved">import</span> org.scalatest.GivenWhenThen
 * <span class="stReserved">import</span> org.scalatestexamples.helpers.Stack
 * <br /><span class="stReserved">class</span> <span class="stType">StackFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> <span class="stReserved">with</span> <span class="stType">GivenWhenThen</span> <span class="stReserved">with</span> <span class="stType">FeatureSpecStackBehaviors</span> {
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
 * <br />  feature(<span class="stQuotedString">"A Stack is pushed and popped"</span>) {
 * <br />    scenario(<span class="stQuotedString">"empty is invoked on an empty stack"</span>) {
 * <br />      given(<span class="stQuotedString">"an empty stack"</span>)
 *       <span class="stReserved">val</span> stack = emptyStack
 * <br />      when(<span class="stQuotedString">"empty is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"empty returns true"</span>)
 *       assert(stack.empty)
 *     }
 * <br />    scenario(<span class="stQuotedString">"peek is invoked on an empty stack"</span>) {
 * <br />      given(<span class="stQuotedString">"an empty stack"</span>)
 *       <span class="stReserved">val</span> stack = emptyStack
 * <br />      when(<span class="stQuotedString">"peek is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"peek throws IllegalStateException"</span>)
 *       intercept[<span class="stType">IllegalStateException</span>] {
 *         stack.peek
 *       }
 *     }
 * <br />    scenario(<span class="stQuotedString">"pop is invoked on an empty stack"</span>) {
 * <br />      given(<span class="stQuotedString">"an empty stack"</span>)
 *       <span class="stReserved">val</span> stack = emptyStack
 * <br />      when(<span class="stQuotedString">"pop is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"pop throws IllegalStateException"</span>)
 *       intercept[<span class="stType">IllegalStateException</span>] {
 *         emptyStack.pop
 *       }
 *     }
 * <br />    scenariosFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 *     scenariosFor(nonFullStack(stackWithOneItem))
 * <br />    scenariosFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
 *     scenariosFor(nonFullStack(stackWithOneItemLessThanCapacity))
 * <br />    scenario(<span class="stQuotedString">"full is invoked on a full stack"</span>) {
 * <br />      given(<span class="stQuotedString">"an full stack"</span>)
 *       <span class="stReserved">val</span> stack = fullStack
 * <br />      when(<span class="stQuotedString">"full is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"full returns true"</span>)
 *       assert(stack.full)
 *     }
 * <br />    scenariosFor(nonEmptyStack(fullStack, lastValuePushed))
 * <br />    scenario(<span class="stQuotedString">"push is invoked on a full stack"</span>) {
 * <br />      given(<span class="stQuotedString">"an full stack"</span>)
 *       <span class="stReserved">val</span> stack = fullStack
 * <br />      when(<span class="stQuotedString">"push is invoked on the stack"</span>)
 *       then(<span class="stQuotedString">"push throws IllegalStateException"</span>)
 *       intercept[<span class="stType">IllegalStateException</span>] {
 *         stack.push(<span class="stLiteral">10</span>)
 *       }
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
 * scala> (new StackFeatureSpec).execute()
 * Feature: A Stack is pushed and popped 
 *   Scenario: empty is invoked on an empty stack
 *     Given an empty stack 
 *     When empty is invoked on the stack 
 *     Then empty returns true 
 *   Scenario: peek is invoked on an empty stack
 *     Given an empty stack 
 *     When peek is invoked on the stack 
 *     Then peek throws IllegalStateException 
 *   Scenario: pop is invoked on an empty stack
 *     Given an empty stack 
 *     When pop is invoked on the stack 
 *     Then pop throws IllegalStateException 
 *   Scenario: empty is invoked on this non-empty stack: Stack(9)
 *     Given a non-empty stack 
 *     When empty is invoked on the stack 
 *     Then empty returns false 
 *   Scenario: peek is invoked on this non-empty stack: Stack(9)
 *     Given a non-empty stack 
 *     When peek is invoked on the stack 
 *     Then peek returns the last item added 
 *     And the size of the stack is the same as before 
 *   Scenario: pop is invoked on this non-empty stack: Stack(9)
 *     Given a non-empty stack 
 *     When pop is invoked on the stack 
 *     Then pop returns the last item added 
 *     And the size of the stack one less than before 
 *   Scenario: full is invoked on this non-full stack: Stack(9)
 *     Given a non-full stack 
 *     When full is invoked on the stack 
 *     Then full returns false 
 *   Scenario: push is invoked on this non-full stack: Stack(9)
 *     Given a non-full stack 
 *     When push is invoked on the stack 
 *     Then the size of the stack is one greater than before 
 *     And the top of the stack contains the pushed value 
 *   Scenario: empty is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 *     Given a non-empty stack 
 *     When empty is invoked on the stack 
 *     Then empty returns false 
 *   Scenario: peek is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 *     Given a non-empty stack 
 *     When peek is invoked on the stack 
 *     Then peek returns the last item added 
 *     And the size of the stack is the same as before 
 *   Scenario: pop is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 *     Given a non-empty stack 
 *     When pop is invoked on the stack 
 *     Then pop returns the last item added 
 *     And the size of the stack one less than before 
 *   Scenario: full is invoked on this non-full stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 *     Given a non-full stack 
 *     When full is invoked on the stack 
 *     Then full returns false 
 *   Scenario: push is invoked on this non-full stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1)
 *     Given a non-full stack 
 *     When push is invoked on the stack 
 *     Then the size of the stack is one greater than before 
 *     And the top of the stack contains the pushed value 
 *   Scenario: full is invoked on a full stack
 *     Given an full stack 
 *     When full is invoked on the stack 
 *     Then full returns true 
 *   Scenario: empty is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 *     Given a non-empty stack 
 *     When empty is invoked on the stack 
 *     Then empty returns false 
 *   Scenario: peek is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 *     Given a non-empty stack 
 *     When peek is invoked on the stack 
 *     Then peek returns the last item added 
 *     And the size of the stack is the same as before 
 *   Scenario: pop is invoked on this non-empty stack: Stack(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
 *     Given a non-empty stack 
 *     When pop is invoked on the stack 
 *     Then pop returns the last item added 
 *     And the size of the stack one less than before 
 *   Scenario: push is invoked on a full stack
 *     Given an full stack 
 *     When push is invoked on the stack 
 *     Then push throws IllegalStateException 
 * </pre>
 * 
 * <p>
 * One thing to keep in mind when using shared tests is that in ScalaTest, each test in a suite must have a unique name.
 * If you register the same tests repeatedly in the same suite, one problem you may encounter is an exception at runtime
 * complaining that multiple tests are being registered with the same test name.
 * In a <code>FeatureSpec</code> there is no nesting construct analogous to <code>Spec</code>'s <code>describe</code> clause.
 * Therefore, you need to do a bit of
 * extra work to ensure that the test names are unique. If a duplicate test name problem shows up in a
 * <code>FeatureSpec</code>, you'll need to pass in a prefix or suffix string to add to each test name. You can pass this string
 * the same way you pass any other data needed by the shared tests, or just call <code>toString</code> on the shared fixture object.
 * This is the approach taken by the previous <code>FeatureSpecStackBehaviors</code> example.
 * </p>
 *
 * <p>
 * Given this <code>FeatureSpecStackBehaviors</code> trait, calling it with the <code>stackWithOneItem</code> fixture, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * scenariosFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
 * </pre><pre class="stHighlighted">
 * scenariosFor(nonEmptyStack(stackWithOneItem, lastValuePushed))
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
 * scenariosFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
 * </pre><pre class="stHighlighted">
 * scenariosFor(nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed))
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
 * A <code>FeatureSpec</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing a <code>FeatureSpec</code>, groups of tests can
 * optionally be included and/or excluded. To tag a <code>FeatureSpec</code>'s tests,
 * you pass objects that extend abstract class <code>org.scalatest.Tag</code> to methods
 * that register tests, <code>test</code> and <code>ignore</code>. Class <code>Tag</code> takes one parameter, a string name.  If you have
 * created Java annotation interfaces for use as group names in direct subclasses of <code>org.scalatest.Suite</code>,
 * then you will probably want to use group names on your <code>FeatureSpec</code>s that match. To do so, simply 
 * pass the fully qualified names of the Java interfaces to the <code>Tag</code> constructor. For example, if you've
 * defined Java annotation interfaces with fully qualified names, <code>com.mycompany.groups.SlowTest</code> and
 * <code>com.mycompany.groups.DbTest</code>, then you could
 * create matching groups for <code>FeatureSpec</code>s like this:
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
 * Given these definitions, you could place <code>FeatureSpec</code> tests into groups like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 *
 * class ArithmeticFeatureSpec extends FeatureSpec {
 *
 *   // Sharing fixture objects via instance variables
 *   val shared = 5
 *
 *   feature("Integer arithmetic") {
 *
 *     scenario("addition", SlowTest) {
 *       val sum = 2 + 3
 *       assert(sum === shared)
 *     }
 *
 *     scenario("subtraction", SlowTest, DbTest) {
 *       val diff = 7 - 2
 *       assert(diff === shared)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <br /><span class="stReserved">class</span> <span class="stType">ArithmeticFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> {
 * <br />  <span class="stLineComment">// Sharing fixture objects via instance variables</span>
 *   <span class="stReserved">val</span> shared = <span class="stLiteral">5</span>
 * <br />  feature(<span class="stQuotedString">"Integer arithmetic"</span>) {
 * <br />    scenario(<span class="stQuotedString">"addition"</span>, <span class="stType">SlowTest</span>) {
 *       <span class="stReserved">val</span> sum = <span class="stLiteral">2</span> + <span class="stLiteral">3</span>
 *       assert(sum === shared)
 *     }
 * <br />    scenario(<span class="stQuotedString">"subtraction"</span>, <span class="stType">SlowTest</span>, <span class="stType">DbTest</span>) {
 *       <span class="stReserved">val</span> diff = <span class="stLiteral">7</span> - <span class="stLiteral">2</span>
 *       assert(diff === shared)
 *     }
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
 * good intention of resurrecting the test at a later time, <code>FeatureSpec</code> provides registration
 * methods that start with <code>ignore</code> instead of <code>scenario</code>. For example, to temporarily
 * disable the test named <code>addition</code>, just change &#8220;<code>scenario</code>&#8221; into &#8220;<code>ignore</code>,&#8221; like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 *
 * class ArithmeticFeatureSpec extends FeatureSpec {
 *
 *   // Sharing fixture objects via instance variables
 *   val shared = 5
 *
 *   feature("Integer arithmetic") {
 *
 *     ignore("addition") {
 *       val sum = 2 + 3
 *       assert(sum === shared)
 *     }
 *
 *     scenario("subtraction") {
 *       val diff = 7 - 2
 *       assert(diff === shared)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <br /><span class="stReserved">class</span> <span class="stType">ArithmeticFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> {
 * <br />  <span class="stLineComment">// Sharing fixture objects via instance variables</span>
 *   <span class="stReserved">val</span> shared = <span class="stLiteral">5</span>
 * <br />  feature(<span class="stQuotedString">"Integer arithmetic"</span>) {
 * <br />    ignore(<span class="stQuotedString">"addition"</span>) {
 *       <span class="stReserved">val</span> sum = <span class="stLiteral">2</span> + <span class="stLiteral">3</span>
 *       assert(sum === shared)
 *     }
 * <br />    scenario(<span class="stQuotedString">"subtraction"</span>) {
 *       <span class="stReserved">val</span> diff = <span class="stLiteral">7</span> - <span class="stLiteral">2</span>
 *       assert(diff === shared)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>ArithmeticFeatureSpec</code> with:
 * </p>
 *
 * <pre>
 * scala> (new ArithmeticFeatureSpec).execute()
 * </pre>
 *
 * <p>
 * It will run only <code>subtraction</code> and report that <code>addition</code> was ignored:
 * </p>
 *
 * <pre>
 * Feature: Integer arithmetic 
 *   Scenario: addition !!! IGNORED !!!
 *   Scenario: subtraction
 * </pre>
 *
 * <h2>Informers</h2>
 *
 * <p>
 * One of the parameters to the primary <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default by <code>FeatureSpec</code>'s methods will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <code>Informer</code> that will forward information to the current <code>Reporter</code>
 * is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via its <code>apply</code> method.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <code>InfoProvided</code> event.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 *
 * class ArithmeticFeatureSpec extends FeatureSpec {
 *
 *   feature("Integer arithmetic") {
 *
 *     scenario("addition") {
 *       val sum = 2 + 3
 *       assert(sum === 5)
 *       info("Addition seems to work")
 *     }
 *
 *     scenario("subtraction") {
 *       val diff = 7 - 2
 *       assert(diff === 5)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <br /><span class="stReserved">class</span> <span class="stType">ArithmeticFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> {
 * <br />  feature(<span class="stQuotedString">"Integer arithmetic"</span>) {
 * <br />    scenario(<span class="stQuotedString">"addition"</span>) {
 *       <span class="stReserved">val</span> sum = <span class="stLiteral">2</span> + <span class="stLiteral">3</span>
 *       assert(sum === <span class="stLiteral">5</span>)
 *       info(<span class="stQuotedString">"Addition seems to work"</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"subtraction"</span>) {
 *       <span class="stReserved">val</span> diff = <span class="stLiteral">7</span> - <span class="stLiteral">2</span>
 *       assert(diff === <span class="stLiteral">5</span>)
 *     }
 *   }
 * }
 * </pre>
 *
 * If you run this <code>ArithmeticFeatureSpec</code> from the interpreter, you will see the following message
 * included in the printed report:
 *
 * <pre>
 * Feature: Integer arithmetic 
 *   Scenario: addition
 *     Addition seems to work 
 * </pre>
 *
 * <p>
 * One use case for the <code>Informer</code> is to pass more information about a scenario to the reporter. For example,
 * the <code>GivenWhenThen</code> trait provides methods that use the implicit <code>info</code> provided by <code>FeatureSpec</code>
 * to pass such information to the reporter. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import org.scalatest.GivenWhenThen
 * 
 * class ArithmeticSpec extends FeatureSpec with GivenWhenThen {
 * 
 *   feature("Integer arithmetic") {
 *
 *     scenario("addition") {
 * 
 *       given("two integers")
 *       val x = 2
 *       val y = 3
 * 
 *       when("they are added")
 *       val sum = x + y
 * 
 *       then("the result is the sum of the two numbers")
 *       assert(sum === 5)
 *     }
 *
 *     scenario("subtraction") {
 * 
 *       given("two integers")
 *       val x = 7
 *       val y = 2
 * 
 *       when("one is subtracted from the other")
 *       val diff = x - y
 * 
 *       then("the result is the difference of the two numbers")
 *       assert(diff === 5)
 *     }
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <span class="stReserved">import</span> org.scalatest.GivenWhenThen
 * <br /><span class="stReserved">class</span> <span class="stType">ArithmeticSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> <span class="stReserved">with</span> <span class="stType">GivenWhenThen</span> {
 * <br />  feature(<span class="stQuotedString">"Integer arithmetic"</span>) {
 * <br />    scenario(<span class="stQuotedString">"addition"</span>) {
 * <br />      given(<span class="stQuotedString">"two integers"</span>)
 *       <span class="stReserved">val</span> x = <span class="stLiteral">2</span>
 *       <span class="stReserved">val</span> y = <span class="stLiteral">3</span>
 * <br />      when(<span class="stQuotedString">"they are added"</span>)
 *       <span class="stReserved">val</span> sum = x + y
 * <br />      then(<span class="stQuotedString">"the result is the sum of the two numbers"</span>)
 *       assert(sum === <span class="stLiteral">5</span>)
 *     }
 * <br />    scenario(<span class="stQuotedString">"subtraction"</span>) {
 * <br />      given(<span class="stQuotedString">"two integers"</span>)
 *       <span class="stReserved">val</span> x = <span class="stLiteral">7</span>
 *       <span class="stReserved">val</span> y = <span class="stLiteral">2</span>
 * <br />      when(<span class="stQuotedString">"one is subtracted from the other"</span>)
 *       <span class="stReserved">val</span> diff = x - y
 * <br />      then(<span class="stQuotedString">"the result is the difference of the two numbers"</span>)
 *       assert(diff === <span class="stLiteral">5</span>)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this <code>FeatureSpec</code> from the interpreter, you will see the following messages
 * included in the printed report:
 * </p>
 *
 * <pre>
 * scala> (new ArithmeticFeatureSpec).execute()
 * Feature: Integer arithmetic 
 *   Scenario: addition
 *     Given two integers 
 *     When they are added 
 *     Then the result is the sum of the two numbers 
 *   Scenario: subtraction
 *     Given two integers 
 *     When one is subtracted from the other 
 *     Then the result is the difference of the two numbers 
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
 * You can mark tests as pending in a <code>FeatureSpec</code> like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 *
 * class ArithmeticFeatureSpec extends FeatureSpec {
 *
 *   // Sharing fixture objects via instance variables
 *   val shared = 5
 *
 *   feature("Integer arithmetic") {
 *
 *     scenario("addition") {
 *       val sum = 2 + 3
 *       assert(sum === shared)
 *     }
 *
 *     scenario("subtraction") (pending)
 *   }
 * }
 * </pre><pre class="stHighlighted">
 * <span class="stReserved">import</span> org.scalatest.FeatureSpec
 * <br /><span class="stReserved">class</span> <span class="stType">ArithmeticFeatureSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> {
 * <br />  <span class="stLineComment">// Sharing fixture objects via instance variables</span>
 *   <span class="stReserved">val</span> shared = <span class="stLiteral">5</span>
 * <br />  feature(<span class="stQuotedString">"Integer arithmetic"</span>) {
 * <br />    scenario(<span class="stQuotedString">"addition"</span>) {
 *       <span class="stReserved">val</span> sum = <span class="stLiteral">2</span> + <span class="stLiteral">3</span>
 *       assert(sum === shared)
 *     }
 * <br />    scenario(<span class="stQuotedString">"subtraction"</span>) (pending)
 *   }
 * }
 * </pre>
 *
 * <p>
 * (Note: "<code>(pending)</code>" is the body of the test. Thus the test contains just one statement, an invocation
 * of the <code>pending</code> method, which throws <code>TestPendingException</code>.)
 * If you run this version of <code>ArithmeticFeatureSpec</code> with:
 * </p>
 *
 * <pre>
 * scala> (new ArithmeticFeatureSpec).execute()
 * </pre>
 *
 * <p>
 * It will run both tests, but report that <code>subtraction</code> is pending. You'll see:
 * </p>
 *
 * <pre>
 * Feature: Integer arithmetic 
 *   Scenario: addition
 *   Scenario: subtraction (pending)
 * </pre>
 * 
 * <p>
 * One difference between an ignored test and a pending one is that an ignored test is intended to be used during a
 * significant refactorings of the code under test, when tests break and you don't want to spend the time to fix
 * all of them immediately. You can mark some of those broken tests as ignored temporarily, so that you can focus the red
 * bar on just failing tests you actually want to fix immediately. Later you can go back and fix the ignored tests.
 * In other words, by ignoring some failing tests temporarily, you can more easily notice failed tests that you actually
 * want to fix. By contrast, a pending test is intended to be used before a test and/or the code under test is written.
 * Pending indicates you've decided to write a test for a bit of behavior, but either you haven't written the test yet, or
 * have only written part of it, or perhaps you've written the test but don't want to implement the behavior it tests
 * until after you've implemented a different bit of behavior you realized you need first. Thus ignored tests are designed
 * to facilitate refactoring of existing code whereas pending tests are designed to facilitate the creation of new code.
 * </p>
 *
 * <p>
 * One other difference between ignored and pending tests is that ignored tests are implemented as a test tag that is
 * excluded by default. Thus an ignored test is never executed. By contrast, a pending test is implemented as a
 * test that throws <code>TestPendingException</code> (which is what calling the <code>pending</code> method does). Thus
 * the body of pending tests are executed up until they throw <code>TestPendingException</code>. The reason for this difference
 * is that it enables your unfinished test to send <code>InfoProvided</code> messages to the reporter before it completes
 * abruptly with <code>TestPendingException</code>, as shown in the previous example on <code>Informer</code>s
 * that used the <code>GivenWhenThen</code> trait. For example, the following snippet in a <code>FeatureSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 *   feature("Integer arithmetic") {
 *
 *     scenario("addition") {
 *       given("two integers")
 *       when("they are added")
 *       then("the result is the sum of the two numbers")
 *       pending
 *     }
 *     // ...
 * </pre><pre class="stHighlighted">
 * feature(<span class="stQuotedString">"Integer arithmetic"</span>) {
 * <br />  scenario(<span class="stQuotedString">"addition"</span>) {
 *     given(<span class="stQuotedString">"two integers"</span>)
 *     when(<span class="stQuotedString">"they are added"</span>)
 *     then(<span class="stQuotedString">"the result is the sum of the two numbers"</span>)
 *     pending
 *   }
 *   <span class="stLineComment">// ...</span>
 * </pre>
 *
 * <p>
 * Would yield the following output when run in the interpreter:
 * </p>
 *
 * <pre>
 * Feature: Integer arithmetic 
 *   Scenario: addition (pending)
 *     Given two integers 
 *     When they are added 
 *     Then the result is the sum of the two numbers 
 * </pre>
 *
 * @author Bill Venners
 */
trait FeatureSpec extends Suite { thisSuite =>

  private final val engine = new Engine("concurrentFeatureSpecMod", "FeatureSpec")
  import engine._

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>FeatureSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def info: Informer = atomicInformer.get

  /**
   * Register a test with the given spec text, optional tags, and test function value that takes no arguments.
   * An invocation of this method is called an &#8220;example.&#8221;
   *
   * This method will register the test for later execution via an invocation of one of the <code>execute</code>
   * methods. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>Spec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullPointerException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  protected def scenario(specText: String, testTags: Tag*)(testFun: => Unit) {

    registerTest(Resources("scenario", specText), testFun _, "scenarioCannotAppearInsideAnotherScenario", "FeatureSpec.scala", "scenario", testTags: _*)
  }

  /**
   * Register a test to ignore, which has the given spec text, optional tags, and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>Spec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullPointerException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  protected def ignore(specText: String, testTags: Tag*)(testFun: => Unit) {
    registerIgnoredTest(Resources("scenario", specText), testFun _, "ignoreCannotAppearInsideAScenario", "FeatureSpec.scala", "ignore", testTags: _*)
  }
  
  /**
   * Describe a &#8220;subject&#8221; being specified and tested by the passed function value. The
   * passed function value may contain more describers (defined with <code>describe</code>) and/or tests
   * (defined with <code>it</code>). This trait's implementation of this method will register the
   * description string and immediately invoke the passed function.
   */
  protected def feature(description: String)(fun: => Unit) {

    if (!currentBranchIsTrunk)
      throw new NotAllowedException(Resources("cantNestFeatureClauses"), getStackDepth("FeatureSpec.scala", "feature"))

    registerNestedBranch(description, None, fun, "featureCannotAppearInsideAScenario", "FeatureSpec.scala", "feature")
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>Spec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>FeatureSpec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to 
   * methods <code>test</code> and <code>ignore</code>. 
   * </p>
   */
  override def tags: Map[String, Set[String]] = atomic.get.tagsMap

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all describers surrounding a test,
   * from outside in, and the test's  spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.)
   *
   * @param testName the name of one test to execute.
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param configMap a <code>Map</code> of properties that can be used by this <code>Spec</code>'s executing tests.
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

    runTestImpl(thisSuite, testName, reporter, stopper, configMap, tracker, false, invokeWithFixture)
  }

  /**
   * Run zero to many of this <code>FeatureSpec</code>'s tests.
   *
   * <p>
   * This method takes a <code>testName</code> parameter that optionally specifies a test to invoke.
   * If <code>testName</code> is <code>Some</code>, this trait's implementation of this method
   * invokes <code>runTest</code> on this object, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> value of the <code>testName</code> <code>Option</code> passed
   *   to this method</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * <p>
   * This method takes a <code>Set</code> of tag names that should be included (<code>tagsToInclude</code>), and a <code>Set</code>
   * that should be excluded (<code>tagsToExclude</code>), when deciding which of this <code>Suite</code>'s tests to execute.
   * If <code>tagsToInclude</code> is empty, all tests will be executed
   * except those those belonging to tags listed in the <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is non-empty, only tests
   * belonging to tags mentioned in <code>tagsToInclude</code>, and not mentioned in <code>tagsToExclude</code>
   * will be executed. However, if <code>testName</code> is <code>Some</code>, <code>tagsToInclude</code> and <code>tagsToExclude</code> are essentially ignored.
   * Only if <code>testName</code> is <code>None</code> will <code>tagsToInclude</code> and <code>tagsToExclude</code> be consulted to
   * determine which of the tests named in the <code>testNames</code> <code>Set</code> should be run. For more information on trait tags, see the main documentation for this trait.
   * </p>
   *
   * <p>
   * If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * invokes <code>testNames</code> on this <code>Suite</code> to get a <code>Set</code> of names of tests to potentially execute.
   * (A <code>testNames</code> value of <code>None</code> essentially acts as a wildcard that means all tests in
   * this <code>Suite</code> that are selected by <code>tagsToInclude</code> and <code>tagsToExclude</code> should be executed.)
   * For each test in the <code>testName</code> <code>Set</code>, in the order
   * they appear in the iterator obtained by invoking the <code>elements</code> method on the <code>Set</code>, this trait's implementation
   * of this method checks whether the test should be run based on the <code>tagsToInclude</code> and <code>tagsToExclude</code> <code>Set</code>s.
   * If so, this implementation invokes <code>runTest</code>, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> name of the test to run (which will be one of the names in the <code>testNames</code> <code>Set</code>)</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
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
    
    runTestsImpl(thisSuite, testName, reporter, stopper, filter, configMap, distributor, tracker, info, false, runTest)
  }

  /**
   * An immutable <code>Set</code> of test names. If this <code>FeatureSpec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space. For example, consider this <code>FeatureSpec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.FeatureSpec
   *
   * class StackSpec extends FeatureSpec {
   *   feature("A Stack") {
   *     scenario("(when not empty) must allow me to pop") {}
   *     scenario("(when not full) must allow me to push") {}
   *   }
   * }
   * </pre><pre class="stHighlighted">
   * <span class="stReserved">import</span> org.scalatest.FeatureSpec
   * <br /><span class="stReserved">class</span> <span class="stType">StackSpec</span> <span class="stReserved">extends</span> <span class="stType">FeatureSpec</span> {
   *   feature(<span class="stQuotedString">"A Stack"</span>) {
   *     scenario(<span class="stQuotedString">"(when not empty) must allow me to pop"</span>) {}
   *     scenario(<span class="stQuotedString">"(when not full) must allow me to push"</span>) {}
   *   }
   * }
   * </pre>
   *
   * <p>
   * Invoking <code>testNames</code> on this <code>Spec</code> will yield a set that contains the following
   * two test name strings:
   * </p>
   *
   * <pre>
   * "A Stack (when not empty) must allow me to pop"
   * "A Stack (when not full) must allow me to push"
   * </pre>
   */
  // override def testNames: Set[String] = ListSet(atomic.get.testsList.map(_.testName): _*)
  override def testNames: Set[String] = {
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }

  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {

    runImpl(thisSuite, testName, reporter, stopper, filter, configMap, distributor, tracker, super.run)
  }

  /**
   * Registers shared scenarios.
   *
   * <p>
   * This method enables the following syntax for shared scenarios in a <code>FeatureSpec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * scenariosFor(nonEmptyStack(lastValuePushed))
   * </pre><pre class="stHighlighted">
   * scenariosFor(nonEmptyStack(lastValuePushed))
   * </pre>
   *
   * <p>
   * This method just provides syntax sugar intended to make the intent of the code clearer.
   * Because the parameter passed to it is
   * type <code>Unit</code>, the expression will be evaluated before being passed, which
   * is sufficient to register the shared scenarios. For examples of shared scenarios, see the
   * <a href="#SharedScenarios">Shared scenarios section</a> in the main documentation for this trait.
   * </p>
   */
  protected def scenariosFor(unit: Unit) {}
}