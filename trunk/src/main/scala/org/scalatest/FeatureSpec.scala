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
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import Suite.anErrorThatShouldCauseAnAbort

/**
 * A suite of tests in which each test represents one <em>scenario</em> of a <em>feature</em>. 
 * <code>FeatureSpec</code> is intended for writing tests that are "higher level" than unit tests, for example, integration
 * tests, functional tests, and acceptance tests. You can use <code>FeatureSpec</code> for unit testing if you prefer, however.
 * 
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Although also usable for unit testing, <code>FeatureSpec</code>'s main intended use is for acceptance testing. It is similar to <code>FunSpec</code> in 
 * its structure, but different in that it only allows one level of nesting (one feature clause that can contain a series of scenario clauses), 
 * and that the words "feature" and "scenario" show up in the report text whereas <code>FunSpec</code>'s "describe" and "it" do not. Because tests are functions, 
 * you can share tests or programmatically register them, but at the cost of one generated class file per test. <code>FeatureSpec</code> brings into scope no 
 * implicit conversions except the one it inherits from <code>Assertions</code>, the implicit conversion that puts <code>===</code> on everything, so the likelyhood for 
 * implicit conversion conflicts is minimized. Compiling a <code>FeatureSpec</code> will generate one extra class file per feature and one per scenario.  
 * </td></tr></table>
 * 
 * Here's an example:
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec
 * 
 * import org.scalatest.FeatureSpec
 * 
 * class SetSpec extends FeatureSpec {
 * 
 *   feature("A Set") {
 *     scenario("When is empty should have size 0") {
 *       assert(Set.empty.size === 0)
 *     }
 *     
 *     scenario("When is empty and head is invoked, should produce NoSuchElementException") {
 *       intercept[NoSuchElementException] {
 *         Set.empty.head
 *       }
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
 * the subject under specification and test is a Set. The feature being specified and tested is 
 * the behavior of a Set when it is empty and head is invoked. With each scenario you provide a
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
 * <pre class="stREPL">
 * scala> new SetSpec execute
 * </pre>
 *
 * <p>
 * You would see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">A Set
 *   Scenario: When is empty should have size 0
 *   Scenario: When is empty and head is invoked, should produce NoSuchElementException
 * </span> 
 * </pre>
 *
 * <p>
 * See also: <a href="http://www.scalatest.org/getting_started_with_feature_spec" target="_blank">Getting started with <code>FeatureSpec</code>.</a>
 * </p>
 * 
 * <p>
 * <em>Note: Trait <code>FeatureSpec</code>'s syntax is in part inspired by <a href="http://cukes.info/" target="_blank">Cucumber</a>, a Ruby BDD framework.</em>
 *</p>
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
 * package org.scalatest.examples.featurespec.ignore
 * 
 * import org.scalatest.FeatureSpec
 * 
 * class SetSpec extends FeatureSpec {
 *   
 *   feature("A Set") {
 *     ignore("When is empty should have size 0") {
 *       assert(Set.empty.size === 0)
 *     }
 *     
 *     scenario("When is empty and head is invoked, should produce NoSuchElementException") {
 *       intercept[NoSuchElementException] {
 *         Set.empty.head
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>SetSpec</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala> new SetSpec execute
 * </pre>
 *
 * <p>
 * It will run only <code>When is empty and head is invoked, should produce NoSuchElementException</code> and report that <code>When is empty should have size 0</code> was ignored:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">A Set</span>
 *   <span class="stYellow">- Scenario: When is empty should have size 0 !!! IGNORED !!!</span>
 *   <span class="stGreen">Scenario: When is empty and head is invoked, should produce NoSuchElementException</span>
 * </pre>
 *
 * <h2>Informers</h2>
 *
 * <p>
 * One of the parameters to the <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default by <code>FeatureSpec</code>'s methods will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <code>Informer</code> that will forward information to the current <code>Reporter</code>
 * is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via its <code>apply</code> method.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <code>InfoProvided</code> event.
 * </p>
 * 
 * <p>
 * One use case for the <code>Informer</code> is to pass more information about a scenario to the reporter. For example,
 * the <code>GivenWhenThen</code> trait provides methods that use the implicit <code>info</code> provided by <code>FeatureSpec</code>
 * to pass such information to the reporter. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.info
 * 
 * import collection.mutable
 * import org.scalatest._
 * 
 * class SetSpec extends FeatureSpec with GivenWhenThen {
 *   
 *   feature("An element can be added to an empty mutable Set") {
 *     scenario("When an element is added to an empty mutable Set") {
 *       given("an empty mutable Set")
 *       val set = mutable.Set.empty[String]
 * 
 *       when("an element is added")
 *       set += "clarity"
 * 
 *       then("the Set should have size 1")
 *       assert(set.size === 1)
 * 
 *       and("the Set should contain the added element")
 *       assert(set.contains("clarity"))
 * 
 *       info("That's all folks!")
 *     }
 *   }
 * }
 * </pre>
 *
 * If you run this <code>SetSpec</code> from the interpreter, you will see the following message
 * included in the printed report:
 *
 * <pre class="stREPL">
 * <span class="stGreen">An element can be added to an empty mutable Set
 *   Scenario: When an element is added to an empty mutable Set
 *     Given an empty mutable Set
 *     When an element is added 
 *     Then the Set should have size 1 
 *     And the Set should contain the added element 
 *     That's all folks!</span> 
 * </pre>
 *
 * <h2>Pending tests</h2>
 *
 * <p>
 * A <em>pending test</em> is one that has been given a name but is not yet implemented. The purpose of
 * pending tests is to facilitate a style of testing in which documentation of behavior is sketched
 * out before tests are written to verify that behavior (and often, before the behavior of
 * the system being tested is itself implemented). Such sketches form a kind of specification of
 * what tests and functionality to implement later.
 * </p>
 *
 * <p>
 * To support this style of testing, a test can be given a name that specifies one
 * bit of behavior required by the system being tested. The test can also include some code that
 * sends more information about the behavior to the reporter when the tests run. At the end of the test,
 * it can call method <code>pending</code>, which will cause it to complete abruptly with <code>TestPendingException</code>.
 * </p>
 *
 * <p>
 * Because tests in ScalaTest can be designated as pending with <code>TestPendingException</code>, both the test name and any information
 * sent to the reporter when running the test can appear in the report of a test run. (In other words,
 * the code of a pending test is executed just like any other test.) However, because the test completes abruptly
 * with <code>TestPendingException</code>, the test will be reported as pending, to indicate
 * the actual test, and possibly the functionality, has not yet been implemented.
 * You can mark tests as pending in a <code>FeatureSpec</code> like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.pending
 * 
 * import org.scalatest._
 * 
 * class SetSpec extends FeatureSpec {
 * 
 *   feature("A Set") {
 *     scenario("When empty should have size 0") (pending)
 *     
 *     scenario("When empty and head is invoked should produce NoSuchElementException") {
 *       intercept[NoSuchElementException] {
 *         Set.empty.head
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * (Note: "<code>(pending)</code>" is the body of the test. Thus the test contains just one statement, an invocation
 * of the <code>pending</code> method, which throws <code>TestPendingException</code>.)
 * If you run this version of <code>ArithmeticSpec</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala> new SetSpec execute
 * </pre>
 *
 * <p>
 * It will run both tests, but report that <code>When empty should have size 0</code> is pending. You'll see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">A Set</span>
 *   <span class="stYellow">Scenario: When empty should have size 0 (pending)</span>
 *   <span class="stGreen">Scenario: When empty and head is invoked should produce NoSuchElementException</span>
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
 *&nbsp;
 *     scenario("addition") {
 *       given("two integers")
 *       when("they are added")
 *       then("the result is the sum of the two numbers")
 *       pending
 *     }
 *     // ...
 * </pre>
 *
 * <p>
 * Would yield the following output when run in the interpreter:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">Integer arithmetic</span> 
 *   <span class="stYellow">Scenario: addition (pending)
 *     Given two integers 
 *     When they are added 
 *     Then the result is the sum of the two numbers</span> 
 * </pre>
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
 * defined Java annotation interfaces with fully qualified names, <code>com.mycompany.tags.SlowTest</code> and
 * <code>com.mycompany.tags.DbTest</code>, then you could
 * create matching groups for <code>FeatureSpec</code>s like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.tagging
 * 
 * import org.scalatest.Tag
 * 
 * object SlowTest extends Tag("com.mycompany.tags.SlowTest")
 * object DbTest extends Tag("com.mycompany.tags.DbTest")
 * </pre>
 *
 * <p>
 * Given these definitions, you could place <code>FeatureSpec</code> tests into groups like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * 
 * class SetSpec extends FeatureSpec {
 * 
 *   feature("A Set") {
 *     scenario("When is empty should have size 0", SlowTest) {
 *       assert(Set.empty.size === 0)
 *     }
 *     
 *     scenario("When is empty and head is invoked, should produce NoSuchElementException", SlowTest, DbTest) {
 *       intercept[NoSuchElementException] {
 *         Set.empty.head
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * This code marks both tests  with the <code>com.mycompany.tags.SlowTest</code> tag, 
 * and test "When 4 - 1 is entered, the result should be 3" with the <code>com.mycompany.tags.DbTest</code> tag.
 * </p>
 *
 * <p>
 * The <code>run</code> method takes a <code>Filter</code>, whose constructor takes an optional
 * <code>Set[String]</code> called <code>tagsToInclude</code> and a <code>Set[String]</code> called
 * <code>tagsToExclude</code>. If <code>tagsToInclude</code> is <code>None</code>, all tests will be run
 * except those those belonging to tags listed in the
 * <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is defined, only tests
 * belonging to tags mentioned in the <code>tagsToInclude</code> set, and not mentioned in <code>tagsToExclude</code>,
 * will be run.
 * </p>
 *
 * <a name="sharedFixtures"></a><h2>Shared fixtures</h2>
 *
 * <p>
 * A test <em>fixture</em> is composed of the objects and other artifacts (files, sockets, database
 * connections, <em>etc.</em>) tests use to do their work.
 * When multiple tests need to work with the same fixtures, it is important to try and avoid
 * duplicating the fixture code across those tests. The more code duplication you have in your
 * tests, the greater drag the tests will have on refactoring the actual production code.
 * ScalaTest recommends several techniques to eliminate such code duplication, and provides several
 * traits to help. Each technique is geared towards helping you reduce code duplication without introducing
 * instance <code>var</code>s, shared mutable objects, or other dependencies between tests. Eliminating shared
 * mutable state across tests will make your test code easier to reason about and more amenable for parallel
 * test execution.
 * </p>
 *
 * <p>
 * The following sections
 * describe these techniques, including explaining the recommended usage
 * for each. But first, here's a table summarizing the options:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">Technique</th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">Recommended uses</th></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#getFixtureMethods">get-fixture methods</a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you need the same mutable fixture objects in multiple tests, and don't need to clean up after.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#fixtureContextObjects">fixture-context objects</a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you need different combinations of mutable fixture objects in different tests, and don't need to clean up after. </td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#oneInstancePerTest"><code>OneInstancePerTest</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when porting JUnit tests to ScalaTest, or if you prefer JUnit's approach to test isolation: running each test in its own instance of the test class.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#withFixtureNoArgTest"><code>withFixture(NoArgTest)</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you need to perform side effects at the beginning and end of all or most tests, or want to stack traits that perform such side-effects.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#loanFixtureMethods">loan-fixture methods</a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when different tests need different fixtures that must be cleaned up afterwords.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when all or most tests need the same fixtures that must be cleaned up afterwords.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#beforeAndAfter"><code>BeforeAndAfter</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you need to perform the same side-effects before and/or after tests, rather than at the beginning or end of tests.</td></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: right"><a href="#composingFixtures"><code>BeforeAndAfterEach</code></a></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">Use when you want to stack traits that perform the same side-effects before and/or after tests, rather than at the beginning or end of tests.</td></td></tr>
 * </table>
 *
 * <a name="getFixtureMethods"></a>
 * <h4>Calling get-fixture methods</h4>
 *
 * <p>
 * If you need to create the same mutable fixture objects in multiple tests, and don't need to clean them up after using them, the simplest approach is to write one or
 * more <em>get-fixture</em> methods. A get-fixture method returns a new instance of a needed fixture object (or an holder object containing
 * multiple fixture objects) each time it is called. You can call a get-fixture method at the beginning of each
 * test that needs the fixture, storing the returned object or objects in local variables. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.getfixture
 * 
 * import org.scalatest.FeatureSpec
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends FeatureSpec {
 * 
 *   def fixture = 
 *     new {
 *       val builder = new StringBuilder("ScalaTest is ")
 *       val buffer = new ListBuffer[String]
 *     }
 *   
 *   feature("Fixtures can be shared") {
 *     scenario("User learns how to share fixtures") {
 *       val f = fixture
 *       f.builder.append("easy!")
 *       assert(f.builder.toString === "ScalaTest is easy!")
 *       assert(f.buffer.isEmpty)
 *       f.buffer += "sweet"
 *     }
 *   
 *     scenario("User enjoys writing tests with shared fixtures") {
 *       val f = fixture
 *       f.builder.append("fun!")
 *       assert(f.builder.toString === "ScalaTest is fun!")
 *       assert(f.buffer.isEmpty)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * The &ldquo;<code>f.</code>&rdquo; in front of each use of a fixture object provides a visual indication of which objects 
 * are part of the fixture, but if you prefer, you can import the the members with &ldquo;<code>import f._</code>&rdquo; and use the names directly.
 * </p>
 *
 * <p>
 * If you need to configure fixture objects differently in different tests, you can pass configuration into the get-fixture method. For example, if you could pass
 * in an initial value for a mutable fixture object as a parameter to the get-fixture method.
 * </p>
 *
 * <a name="fixtureContextObjects"></a>
 * <h4>Instantiating fixture-context objects </h4>
 *
 * <p>
 * A alternate technique that is especially useful when different tests need different combinations of fixture objects is to define the fixture objects as instance variables
 * of <em>fixture-context objects</em> whose instantiation forms the body of tests. Like get-fixture methods, fixture-context objects are anly
 * appropriate if you don't need to clean up the fixtures after using them.
 * </p>
 *
 * To use this technique, you define instance variables intialized with fixture objects in traits and/or classes, then in each test instantiate an object that
 * contains just the fixture objects needed by the test. Keep in mind that traits allow you to mix together just the fixture objects needed by each test, whereas classes
 * allow you to pass data in via a constructor to configure the fixture objects. Here's an example in which fixture objects are partitioned into two traits
 * and each test just gets mixes together the traits it needs:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.fixturecontext
 * 
 * import collection.mutable.ListBuffer
 * import org.scalatest.FeatureSpec
 * 
 * class ExampleSpec extends FeatureSpec {
 * 
 *   trait Builder {
 *     val builder = new StringBuilder("ScalaTest is ")
 *   }
 * 
 *   trait Buffer {
 *     val buffer = ListBuffer("ScalaTest", "is")
 *   }
 * 
 *   feature("Fixtures can be shared") {
 *     // This test needs the StringBuilder fixture
 *     scenario("User should be productive when writes tests") {
 *       new Builder {
 *         builder.append("productive!")
 *         assert(builder.toString === "ScalaTest is productive!")
 *       }
 *     }
 *     
 *     // This test needs the ListBuffer[String] fixture
 *     scenario("User can write readable test code") {
 *       new Buffer {
 *         buffer += ("readable!")
 *         assert(buffer === List("ScalaTest", "is", "readable!"))
 *       }
 *     }
 * 
 *     // This test needs both the StringBuilder and ListBuffer
 *     scenario("User's test code should be clear and concise") {
 *       new Builder with Buffer {
 *         builder.append("clear!")
 *         buffer += ("concise!")
 *         assert(builder.toString === "ScalaTest is clear!")
 *         assert(buffer === List("ScalaTest", "is", "concise!"))
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <a name="oneInstancePerTest"></a>
 * <h4>Mixing in <code>OneInstancePerTest</code></h4>
 *
 * <p>
 * If every test method requires the same set of
 * mutable fixture objects, and none require cleanup, one other approach you can take is make them simply <code>val</code>s and mix in trait
 * <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>.  If you mix in <code>OneInstancePerTest</code>, each test
 * will be run in its own instance of the <code>Suite</code>, similar to the way JUnit tests are executed. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.oneinstancepertest
 * 
 * import org.scalatest._
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSuite extends FeatureSpec with OneInstancePerTest {
 * 
 *   val builder = new StringBuilder("ScalaTest is ")
 *   val buffer = new ListBuffer[String]
 * 
 *   feature("Fixtures can be shared") {
 *     scenario("User learns how to share fixtures") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     scenario("User enjoys reading tests with shared fixture") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *     } 
 *   }
 * }
 * </pre>
 *
 * <p>
 * One way to think of <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a> is that the entire <code>Suite</code> instance is like a fixture-context object,
 * but with the difference that the test code doesn't run during construction as it does with the real fixture-context object technique. Because this trait emulates JUnit's manner
 * of running tests, this trait can be helpful when porting JUnit tests to ScalaTest. The primary intended use of <code>OneInstancePerTest</code> is to serve as a supertrait for
 * <a href="ParallelTestExecution.html"><code>ParallelTestExecution</code></a> and the <a href="path/package.html">path traits</a>, but you can also mix it in
 * directly to help you port JUnit tests to ScalaTest or if you prefer JUnit's approach to test isolation.
 * </p>
 *
 * <a name="withFixtureNoArgTest"></a>
 * <h4>Overriding <code>withFixture(NoArgTest)</code></h4>
 *
 * <p>
 * Although the get-fixture method, fixture-context object, and <code>OneInstancePerTest</code> approaches take care of setting up a fixture at the beginning of each
 * test, they don't address the problem of cleaning up a fixture at the end of the test. If you just need to perform a side-effect at the beginning or end of
 * a test, and don't need to actually pass any fixture objects into the test, you can override <code>withFixture(NoArgTest)</code>, one of ScalaTest's
 * lifecycle methods defined in trait <a href="AbstractSuite.html"><code>AbstractSuite</code></a>.
 * </p>
 *
 * <p>
 * Trait <code>Suite</code>'s implementation of <code>runTest</code> passes a no-arg test function to <code>withFixture(NoArgTest)</code>. It is <code>withFixture</code>'s
 * responsibility to invoke that test function. <code>Suite</code>'s implementation of <code>withFixture</code> simply
 * invokes the function, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Default implementation in trait Suite
 * protected def withFixture(test: NoArgTest) {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * You can, therefore, override <code>withFixture</code> to perform setup before and/or cleanup after invoking the test function. If
 * you have cleanup to perform, you should invoke the test function inside a <code>try</code> block and perform the cleanup in
 * a <code>finally</code> clause, because the exception that causes a test to fail will propagate through <code>withFixture</code> back
 * to <code>runTest</code>. (In other words, if the test fails, the test function invoked by <code>withFixture</code> will throw an exception.)
 * </p>
 *
 * <p>
 * The <code>withFixture</code> method is designed to be stacked, and to enable this, you should always call the <code>super</code> implementation
 * of <code>withFixture</code>, and let it invoke the test function rather than invoking the test function directly. In other words, instead of writing
 * &ldquo;<code>test()</code>&rdquo;, you should write &ldquo;<code>super.withFixture(test)</code>&rdquo;, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withFixture(test: NoArgTest) {
 *   // Perform setup
 *   try {
 *     super.withFixture(test) // Invoke the test function
 *   }
 *   finally {
 *     // Perform cleanup
 *   }
 * }
 * </pre>
 *
 * <p>
 * Here's an example in which <code>withFixture(NoArgTest)</code> is used to take a snapshot of the working directory if a test fails, and 
 * and send that information to the reporter:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.noargtest
 * 
 * import java.io.File
 * import org.scalatest.FeatureSpec
 * 
 * class ExampleSpec extends FeatureSpec {
 * 
 *   final val tmpDir = "tmpDir"
 * 
 *   override def withFixture(test: NoArgTest) {
 * 
 *     try {
 *       super.withFixture(test)
 *     }
 *     catch {
 *       case e: Exception =>
 *         val currDir = new File(".")
 *         val fileNames = currDir.list()
 *         info("Dir snapshot: " + fileNames.mkString(", "))
 *         throw e
 *     }
 *   }
 * 
 *   feature("Calculator Add") {
 *     scenario("1 + 1 should be 2") {
 *       assert(1 + 1 === 2)
 *     }
 * 
 *     scenario("2 + 2 should be 4") {
 *       assert(2 + 2 === 3)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running this version of <code>ExampleSuite</code> in the interpreter in a directory with two files, <code>hello.txt</code> and <code>world.txt</code>
 * would give the following output:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new ExampleSuite execute
 * <span class="stGreen">ExampleSuite:
 * Calculator Add
 * Scenario: 1 + 1 should be 2
 * <span class="stRed">Scenario: 2 + 2 should be 4 *** FAILED ***
 * 4 did not equal 3 (<console>:82)
 *   + Dir snapshot: hello.txt, world.txt </span>
 * </pre>
 *
 * <p>
 * Note that the <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a> passed to <code>withFixture</code>, in addition to
 * an <code>apply</code> method that executes the test, also includes the test name and the <a href="#configMapSection">config
 * map</a> passed to <code>runTest</code>. Thus you can also use the test name and configuration objects in your <code>withFixture</code>
 * implementation.
 * </p>
 *
 * <a name="loanFixtureMethods"></a>
 * <h4>Calling loan-fixture methods</h4>
 *
 * <p>
 * If you need to both pass a fixture object into a test <em>and</em> and perform cleanup at the end of the test, you'll need to use the <em>loan pattern</em>.
 * If different tests need different fixtures that require cleanup, you can implement the loan pattern directly by writing <em>loan-fixture</em> methods.
 * A loan-fixture method takes a function whose body forms part or all of a test's code. It creates a fixture, passes it to the test code by invoking the
 * function, then cleans up the fixture after the function returns.
 * </p>
 *
 * <p>
 * The following example shows three tests that use two fixtures, a database and a file. Both require cleanup after, so each is provided via a
 * loan-fixture method. (In this example, the database is simulated with a <code>StringBuffer</code>.)
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.loanfixture
 * 
 * import java.util.concurrent.ConcurrentHashMap
 * 
 * object DbServer { // Simulating a database server
 *   type Db = StringBuffer
 *   private val databases = new ConcurrentHashMap[String, Db]
 *   def createDb(name: String): Db = {
 *     val db = new StringBuffer
 *     databases.put(name, db)
 *     db
 *   }
 *   def removeDb(name: String) {
 *     databases.remove(name)
 *   }
 * }
 * 
 * import org.scalatest.FeatureSpec
 * import DbServer._
 * import java.util.UUID.randomUUID
 * import java.io._
 * 
 * class ExampleSpec extends FeatureSpec {
 * 
 *   def withDatabase(testCode: Db => Any) {
 *     val dbName = randomUUID.toString
 *     val db = createDb(dbName) // create the fixture
 *     try {
 *       db.append("ScalaTest is ") // perform setup
 *       testCode(db) // "loan" the fixture to the test
 *     }
 *     finally {
 *       removeDb(dbName) // clean up the fixture
 *     }
 *   }
 * 
 *   def withFile(testCode: (File, FileWriter) => Any) {
 *     val file = File.createTempFile("hello", "world") // create the fixture
 *     val writer = new FileWriter(file)
 *     try {
 *       writer.write("ScalaTest is ") // set up the fixture
 *       testCode(file, writer) // "loan" the fixture to the test
 *     }
 *     finally {
 *       writer.close() // clean up the fixture
 *     }
 *   }
 * 
 *   feature("Fixtures can be shared") {
 *     // This test needs the file fixture
 *     scenario("User should be productive when writes tests") {
 *       withFile { (file, writer) =>
 *         writer.write("productive!")
 *         writer.flush()
 *         assert(file.length === 24)
 *       }
 *     }
 *     // This test needs the database fixture
 *     scenario("User can write readable test code") {
 *       withDatabase { db =>
 *         db.append("readable!")
 *         assert(db.toString === "ScalaTest is readable!")
 *       }
 *     }
 *     // This test needs both the file and the database
 *     scenario("User's test code should be clear and concise") {
 *       withDatabase { db =>
 *         withFile { (file, writer) => // loan-fixture methods compose
 *           db.append("clear!")
 *           writer.write("concise!")
 *           writer.flush()
 *           assert(db.toString === "ScalaTest is clear!")
 *           assert(file.length === 21)
 *         }
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * As demonstrated by the last test, loan-fixture methods compose. Not only do loan-fixture methods allow you to
 * give each test the fixture it needs, they allow you to give a test multiple fixtures and clean everything up afterwords.
 * </p>
 *
 * <p>
 * Also demonstrated in this example is the technique of giving each test its own "fixture sandbox" to play in. When your fixtures
 * involve external side-effects, like creating files or databases, it is a good idea to give each file or database a unique name as is
 * done in this example. This keeps tests completely isolated, allowing you to run them in parallel if desired.
 * </p>
 *
 * </pre>
 * <a name="withFixtureOneArgTest"></a>
 * <h4>Overriding <code>withFixture(OneArgTest)</code></h4>
 *
 * <p>
 * If all or most tests need the same fixture, you can avoid some of the boilerplate of the loan-fixture method approach by using a <code>fixture.Suite</code>
 * and overriding <code>withFixture(OneArgTest)</code>.
 * Each test in a <code>fixture.Suite</code> takes a fixture as a parameter, allowing you to pass the fixture into
 * the test. You must indicate the type of the fixture parameter by specifying <code>FixtureParam</code>, and implement a
 * <code>withFixture</code> method that takes a <code>OneArgTest</code>. This <code>withFixture</code> method is responsible for
 * invoking the one-arg test function, so you can perform fixture set up before, and clean up after, invoking and passing
 * the fixture into the test function.
 * </p>
 *
 * <p>
 * To enable the stacking of traits that define <code>withFixture(NoArgTest)</code>, it is a good idea to let
 * <code>withFixture(NoArgTest)</code> invoke the test function instead of invoking the test
 * function directly. To do so, you'll need to convert the <code>OneArgTest</code> to a <code>NoArgTest</code>. You can do that by passing
 * the fixture object to the <code>toNoArgTest</code> method of <code>OneArgTest</code>. In other words, instead of
 * writing &ldquo;<code>test(theFixture)</code>&rdquo;, you'd delegate responsibility for
 * invoking the test function to the <code>withFixture(NoArgTest)</code> method of the same instance by writing:
 * </p>
 *
 * <pre>
 * withFixture(test.toNoArgTest(theFixture))
 * </pre>
 *
 * <p>
 * Here's a complete example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.oneargtest
 * 
 * import org.scalatest.fixture
 * import java.io._
 * 
 * class ExampleSpec extends fixture.FeatureSpec {
 * 
 *   case class F(file: File, writer: FileWriter)
 *   type FixtureParam = F
 * 
 *   def withFixture(test: OneArgTest) {
 *     val file = File.createTempFile("hello", "world") // create the fixture
 *     val writer = new FileWriter(file)
 *     try {
 *       writer.write("ScalaTest is ") // set up the fixture
 *       withFixture(test.toNoArgTest(F(file, writer))) // "loan" the fixture to the test
 *     }
 *     finally {
 *       writer.close() // clean up the fixture
 *     }
 *   }
 * 
 *   feature("Testing") {
 *     scenario("User can write test code easily") { f =>
 *       f.writer.write("easy!")
 *       f.writer.flush()
 *       assert(f.file.length === 18)
 *     }
 * 
 *     scenario("User's test code should be fun to read") { f =>
 *       f.writer.write("fun!")
 *       f.writer.flush()
 *       assert(f.file.length === 17)
 *     }
 *   } 
 * }
 * </pre>
 *
 * <p>
 * In this example, the tests actually required two fixture objects, a <code>File</code> and a <code>FileWriter</code>. In such situations you can
 * simply define the <code>FixtureParam</code> type to be a tuple containing the objects, or as is done in this example, a case class containing
 * the objects.  For more information on the <code>withFixture(OneArgTest)</code> technique, see the <a href="fixture/Suite.html">documentation for <code>fixture.Suite</code></a>.
 * </p>
 *
 * <a name="beforeAndAfter"></a>
 * <h4>Mixing in <code>BeforeAndAfter</code></h4>
 *
 * <p>
 * In all the shared fixture examples shown so far, the activities of creating, setting up, and cleaning up the fixture objects have been
 * performed <em>during</em> the test.  This means that if an exception occurs during any of these activities, it will be reported as a test failure.
 * Sometimes, however, you may want setup to happen <em>before</em> the test starts, and cleanup <em>after</em> the test has completed, so that if an
 * exception occurs during setup or cleanup, the entire suite aborts and no more tests are attempted. The simplest way to accomplish this in ScalaTest is
 * to mix in trait <a href="BeforeAndAfter.html"><code>BeforeAndAfter</code></a>.  With this trait you can denote a bit of code to run before each test
 * with <code>before</code> and/or after each test each test with <code>after</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.beforeandafter
 * 
 * import org.scalatest.FeatureSpec
 * import org.scalatest.BeforeAndAfter
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends FeatureSpec with BeforeAndAfter {
 * 
 *   val builder = new StringBuilder
 *   val buffer = new ListBuffer[String]
 * 
 *   before {
 *     builder.append("ScalaTest is ")
 *   }
 * 
 *   after {
 *     builder.clear()
 *     buffer.clear()
 *   }
 * 
 *   feature("Testing") {
 *     scenario("user can write test code easily") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     scenario("user's test code should be fun to read") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Note that the only way <code>before</code> and <code>after</code> code can communicate with test code is via some side-effecting mechanism, commonly by
 * reassigning instance <code>var</code>s or by changing the state of mutable objects held from instance <code>val</code>s (as in this example). If using
 * instance <code>var</code>s or mutable objects held from instance <code>val</code>s you wouldn't be able to run tests in parallel in the same instance
 * of the test class unless you synchronized access to the shared, mutable state. This is why ScalaTest's <code>ParallelTestExecution</code> trait extends
 * <code>OneInstancePerTest</code>. By running each test in its own instance of the class, each test has its own copy of the instance variables, so you
 * don't need to synchronize. If you mixed <code>ParallelTestExecution</code> into the <code>ExampleSuite</code> above, the tests would run in parallel just fine
 * without any synchronization needed on the mutable <code>StringBuilder</code> and <code>ListBuffer[String]</code> objects.
 * </p>
 *
 * <p>
 * Although <code>BeforeAndAfter</code> provides a minimal-boilerplate way to execute code before and after tests, it isn't designed to enable stackable
 * traits, because the order of execution would be non-obvious.  If you want to factor out before and after code that is common to multiple test suites, you 
 * should use trait <code>BeforeAndAfterEach</code> instead, as shown later in the next section,
 * <a href="#composingFixtures.html">composing fixtures by stacking traits</a>.
 * </p>
 *
 * <a name="composingFixtures"></a><h2>Composing fixtures by stacking traits</h2>
 *
 * <p>
 * In larger projects, teams often end up with several different fixtures that test classes need in different combinations,
 * and possibly initialized (and cleaned up) in different orders. A good way to accomplish this in ScalaTest is to factor the individual
 * fixtures into traits that can be composed using the <em>stackable trait</em> pattern. This can be done, for example, by placing
 * <code>withFixture</code> methods in several traits, each of which call <code>super.withFixture</code>. Here's an example in
 * which the <code>StringBuilder</code> and <code>ListBuffer[String]</code> fixtures used in the previous examples have been
 * factored out into two <em>stackable fixture traits</em> named <code>Builder</code> and <code>Buffer</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.composingwithfixture
 * 
 * import org.scalatest._
 * import org.scalatest.AbstractSuite
 * import collection.mutable.ListBuffer
 * 
 * trait Builder extends AbstractSuite { this: Suite =>
 * 
 *   val builder = new StringBuilder
 * 
 *   abstract override def withFixture(test: NoArgTest) {
 *     builder.append("ScalaTest is ")
 *     try {
 *       super.withFixture(test) // To be stackable, must call super.withFixture
 *     }
 *     finally {
 *       builder.clear()
 *     }
 *   }
 * }
 * 
 * trait Buffer extends AbstractSuite { this: Suite =>
 * 
 *   val buffer = new ListBuffer[String]
 * 
 *   abstract override def withFixture(test: NoArgTest) {
 *     try {
 *       super.withFixture(test) // To be stackable, must call super.withFixture
 *     }
 *     finally {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSpec extends FeatureSpec with Builder with Buffer {
 * 
 *   feature("Testing") {
 *     scenario("User can write test code easily") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     scenario("User's test code should be fun to read") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *       buffer += "clear"
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * By mixing in both the <code>Builder</code> and <code>Buffer</code> traits, <code>ExampleSuite</code> gets both fixtures, which will be
 * initialized before each test and cleaned up after. The order the traits are mixed together determines the order of execution.
 * In this case, <code>Builder</code> is "super" to </code>Buffer</code>. If you wanted <code>Buffer</code> to be "super"
 * to <code>Builder</code>, you need only switch the order you mix them together, like this: 
 * </p>
 *
 * <pre class="stHighlight">
 * class Example2Suite extends Suite with Buffer with Builder
 * </pre>
 *
 * <p>
 * And if you only need one fixture you mix in only that trait:
 * </p>
 *
 * <pre class="stHighlight">
 * class Example3Suite extends Suite with Builder
 * </pre>
 *
 * <p>
 * Another way to create stackable fixture traits is by extending the <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a>
 * and/or <a href="BeforeAndAfterAll.html"><code>BeforeAndAfterAll</code></a> traits.
 * <code>BeforeAndAfterEach</code> has a <code>beforeEach</code> method that will be run before each test (like JUnit's <code>setUp</code>),
 * and an <code>afterEach</code> method that will be run after (like JUnit's <code>tearDown</code>).
 * Similarly, <code>BeforeAndAfterAll</code> has a <code>beforeAll</code> method that will be run before all tests,
 * and an <code>afterAll</code> method that will be run after all tests. Here's what the previously shown example would look like if it
 * were rewritten to use the <code>BeforeAndAfterEach</code> methods instead of <code>withFixture</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.featurespec.composingbeforeandaftereach
 * 
 * import org.scalatest._
 * import org.scalatest.BeforeAndAfterEach
 * import collection.mutable.ListBuffer
 * 
 * trait Builder extends BeforeAndAfterEach { this: Suite =>
 * 
 *   val builder = new StringBuilder
 * 
 *   override def beforeEach() {
 *     builder.append("ScalaTest is ")
 *     super.beforeEach() // To be stackable, must call super.beforeEach
 *   }
 * 
 *   override def afterEach() {
 *     try {
 *       super.afterEach() // To be stackable, must call super.afterEach
 *     }
 *     finally {
 *       builder.clear()
 *     }
 *   }
 * }
 * 
 * trait Buffer extends BeforeAndAfterEach { this: Suite =>
 * 
 *   val buffer = new ListBuffer[String]
 * 
 *   override def afterEach() {
 *     try {
 *       super.afterEach() // To be stackable, must call super.afterEach
 *     }
 *     finally {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSpec extends FeatureSpec with Builder with Buffer {
 * 
 *   feature("Testing") {
 *     scenario("User can write test code easily") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     scenario("User's test code should be fun to read") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *       buffer += "clear"
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * To get the same ordering as <code>withFixture</code>, place your <code>super.beforeEach</code> call at the end of each
 * <code>beforeEach</code> method, and the <code>super.afterEach</code> call at the beginning of each <code>afterEach</code>
 * method, as shown in the previous example. It is a good idea to invoke <code>super.afterEach</code> in a <code>try</code>
 * block and perform cleanup in a <code>finally</code> clause, as shown in the previous example, because this ensures the
 * cleanup code is performed even if <code>super.afterEach</code> throws an exception.
 * </p>
 *
 * <p>
 * The difference between stacking traits that extend <code>BeforeAndAfterEach</code> versus traits that implement <code>withFixture</code> is
 * that setup and cleanup code happens before and after the test in <code>BeforeAndAfterEach</code>, but at the beginning and
 * end of the test in <code>withFixture</code>. Thus if a <code>withFixture</code> method completes abruptly with an exception, it is
 * considered a failed test. By contrast, if any of the <code>beforeEach</code> or <code>afterEach</code> methods of <code>BeforeAndAfterEach</code> 
 * complete abruptly, it is considered a failed suite, which will result in a <a href="events/SuiteAborted.html"><code>SuiteAborted</code></a> event.
 * </p>
 * 
 * <a name="sharedFixtures"></a><h2>OLD ONE FOR REFERENCE Shared fixtures</h2>
 *
 * <p>
 * A test <em>fixture</em> is objects or other artifacts (such as files, sockets, database
 * connections, <em>etc.</em>) used by tests to do their work.
 * If a fixture is used by only one test method, then the definitions of the fixture objects can
 * be local to the method, such as the objects assigned to <code>sum</code> and <code>diff</code> in the
 * previous <code>ExampleSpec</code> examples. If multiple methods need to share an immutable fixture, one approach
 * is to assign them to instance variables.
 * </p>
 *
 * <p>
 * In some cases, however, shared <em>mutable</em> fixture objects may be changed by test methods such that
 * they need to be recreated or reinitialized before each test. Shared resources such
 * as files or database connections may also need to 
 * be created and initialized before, and cleaned up after, each test. JUnit 3 offered methods <code>setUp</code> and
 * <code>tearDown</code> for this purpose. In ScalaTest, you can use the <code>BeforeAndAfterEach</code> trait,
 * which will be described later, to implement an approach similar to JUnit's <code>setUp</code>
 * and <code>tearDown</code>, however, this approach usually involves reassigning <code>var</code>s or mutating objects
 * between tests. Before going that route, you may wish to consider some more functional approaches that
 * avoid side effects.
 * </p>
 *
 * <h4>Calling create-fixture methods</h4>
 *
 * <p>
 * One approach is to write one or more <em>create-fixture</em> methods
 * that return a new instance of a needed fixture object (or an holder object containing multiple needed fixture objects) each time it
 * is called. You can then call a create-fixture method at the beginning of each
 * test method that needs the fixture, storing the returned object or objects in local variables. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import collection.mutable.ListBuffer
 *
 * class ExampleSpec extends FeatureSpec {
 * 
 *   def fixture =
 *     new {
 *       val builder = new StringBuilder("ScalaTest is ")
 *       val buffer = new ListBuffer[String]
 *     }
 * 
 *   feature("Fixtures can be shared") {
 * 
 *     scenario("user learns how to share fixtures") {
 *       val f = fixture
 *       f.builder.append("easy!")
 *       assert(f.builder.toString === "ScalaTest is easy!")
 *       assert(f.buffer.isEmpty)
 *       f.buffer += "sweet"
 *     }
 * 
 *     scenario("user enjoys writing tests with shared fixtures") {
 *       val f = fixture
 *       f.builder.append("fun!")
 *       assert(f.builder.toString === "ScalaTest is fun!")
 *       assert(f.buffer.isEmpty)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * The &ldquo;<code>f.</code>&rdquo; in front of each use of a fixture object provides a visual indication of which objects 
 * are part of the fixture, but if you prefer, you can import the the members with &ldquo;<code>import f._</code>&rdquo; and use the names directly.
 * </p>
 *
 * <h4>Instantiating fixture traits</h4>
 *
 * <p>
 * A related technique is to place
 * the fixture objects in a <em>fixture trait</em> and run your test code in the context of a new anonymous class instance that mixes in
 * the fixture trait, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends FeatureSpec {
 * 
 *   trait Fixture {
 *     val builder = new StringBuilder("ScalaTest is ")
 *     val buffer = new ListBuffer[String]
 *   }
 * 
 *   feature("Fixtures can be shared") {
 * 
 *     scenario("user learns how to share fixtures") {
 *       new Fixture {
 *         builder.append("easy!")
 *         assert(builder.toString === "ScalaTest is easy!")
 *         assert(buffer.isEmpty)
 *         buffer += "sweet"
 *       }
 *     }
 * 
 *     scenario("user enjoys writing tests with shared fixtures") {
 *       new Fixture {
 *         builder.append("fun!")
 *         assert(builder.toString === "ScalaTest is fun!")
 *         assert(buffer.isEmpty)
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <h4>Mixing in <code>OneInstancePerTest</code></h4>
 *
 * <p>
 * If every test method requires the same set of
 * mutable fixture objects, one other approach you can take is make them simply <code>val</code>s and mix in trait
 * <a href="OneInstancePerTest.html"><code>OneInstancePerTest</code></a>.  If you mix in <code>OneInstancePerTest</code>, each test
 * will be run in its own instance of the <code>Suite</code>, similar to the way JUnit tests are executed. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import org.scalatest.OneInstancePerTest
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends FeatureSpec with OneInstancePerTest {
 * 
 *   val builder = new StringBuilder("ScalaTest is ")
 *   val buffer = new ListBuffer[String]
 * 
 *   feature("Fixtures can be shared") {
 * 
 *     scenario("user learns how to share fixtures") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     scenario("user enjoys writing tests with shared fixtures") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Although the create-fixture, fixture-trait, and <code>OneInstancePerTest</code> approaches take care of setting up a fixture before each
 * test, they don't address the problem of cleaning up a fixture after the test completes. In this situation, you'll need to either
 * use side effects or the <em>loan pattern</em>.
 * </p>
 *
 * <h4>Mixing in <code>BeforeAndAfter</code></h4>
 *
 * <p>
 * One way to use side effects is to mix in the <a href="BeforeAndAfter.html"><code>BeforeAndAfter</code></a> trait.
 * With this trait you can denote a bit of code to run before each test with <code>before</code> and/or after each test
 * each test with <code>after</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import org.scalatest.BeforeAndAfter
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends FeatureSpec with BeforeAndAfter {
 * 
 *   val builder = new StringBuilder
 *   val buffer = new ListBuffer[String]
 * 
 *   before {
 *     builder.append("ScalaTest is ")
 *   }
 * 
 *   after {
 *     builder.clear()
 *     buffer.clear()
 *   }
 * 
 *   feature("Fixtures can be shared") {
 * 
 *     scenario("user learns how to share fixtures") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     scenario("user enjoys writing tests with shared fixtures") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *     }
 *   }
 * }
 * </pre>
 * 
 * <h4>Overriding <code>withFixture(NoArgTest)</code></h4>
 *
 * <p>
 * An alternate way to take care of setup and cleanup via side effects
 * is to override <code>withFixture</code>. Trait <code>Suite</code>'s implementation of
 * <code>runTest</code>, which is inherited by this trait, passes a no-arg test function to <code>withFixture</code>. It is <code>withFixture</code>'s
 * responsibility to invoke that test function.  <code>Suite</code>'s implementation of <code>withFixture</code> simply
 * invokes the function, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // Default implementation
 * protected def withFixture(test: NoArgTest) {
 *   test()
 * }
 * </pre>
 *
 * <p>
 * You can, therefore, override <code>withFixture</code> to perform setup before, and cleanup after, invoking the test function. If
 * you have cleanup to perform, you should invoke the test function
 * inside a <code>try</code> block and perform the cleanup in a <code>finally</code> clause.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import collection.mutable.ListBuffer
 *
 * class ExampleSpec extends FeatureSpec {
 *
 *   val builder = new StringBuilder
 *   val buffer = new ListBuffer[String]
 *
 *   override def withFixture(test: NoArgTest) {
 *     builder.append("ScalaTest is ") // perform setup
 *     try {
 *       test() // invoke the test function
 *     }
 *     finally {
 *       builder.clear() // perform cleanup
 *       buffer.clear()
 *     }
 *   }
 *
 *   feature("Fixtures can be shared") {
 * 
 *     scenario("user learns how to share fixtures") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 *
 *     scenario("user enjoys writing tests with shared fixtures") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *       buffer += "clear"
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Note that the <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a> passed to <code>withFixture</code>, in addition to
 * an <code>apply</code> method that executes the test, also includes the test name as well as the <a href="Suite.html#configMapSection">config
 * map</a> passed to <code>runTest</code>. Thus you can also use the test name and configuration objects in <code>withFixture</code>.
 * </p>
 *
 * <p>
 * The reason you should perform cleanup in a <code>finally</code> clause is that <code>withFixture</code> is called by
 * <code>runTest</code>, which expects an exception to be thrown to indicate a failed test. Thus when you invoke
 * the <code>test</code> function inside <code>withFixture</code>, it may complete abruptly with an exception. The <code>finally</code>
 * clause will ensure the fixture cleanup happens as that exception propagates back up the call stack to <code>runTest</code>.
 * </p>
 *
 * <h4>Overriding <code>withFixture(OneArgTest)</code></h4>
 *
 * <p>
 * To use the loan pattern, you can extend <code>fixture.FeatureSpec</code> (from the <code>org.scalatest.fixture</code> package) instead of
 * <code>FeatureSpec</code>. Each test in a <code>fixture.FeatureSpec</code> takes a fixture as a parameter, allowing you to pass the fixture into
 * the test. You must indicate the type of the fixture parameter by specifying <code>FixtureParam</code>, and implement a
 * <code>withFixture</code> method that takes a <code>OneArgTest</code>. This <code>withFixture</code> method is responsible for
 * invoking the one-arg test function, so you can perform fixture set up before, and clean up after, invoking and passing
 * the fixture into the test function. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.fixture
 * import java.io.FileWriter
 * import java.io.File
 * 
 * class ExampleSpec extends fixture.FeatureSpec {
 * 
 *   final val tmpFile = "temp.txt"
 * 
 *   type FixtureParam = FileWriter
 * 
 *   def withFixture(test: OneArgTest) {
 * 
 *     val writer = new FileWriter(tmpFile) // set up the fixture
 *     try {
 *       test(writer) // "loan" the fixture to the test
 *     }
 *     finally {
 *       writer.close() // clean up the fixture
 *     }
 *   }
 * 
 *   feature("Fixtures can be shared") {
 * 
 *     scenario("user learns how to share fixtures") { writer =>
 *       writer.write("Hello, test!")
 *       writer.flush()
 *       assert(new File(tmpFile).length === 12)
 *     }
 * 
 *     scenario("user enjoys writing tests with shared fixtures") { writer =>
 *       writer.write("Hi, test!")
 *       writer.flush()
 *       assert(new File(tmpFile).length === 9)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * For more information, see the <a href="fixture/FeatureSpec.html">documentation for <code>fixture.FeatureSpec</code></a>.
 * </p>
 *
 * <a name="differentFixtures"></a><h2>Providing different fixtures to different tests</h2>
 * 
 * <p>
 * If different tests in the same <code>FeatureSpec</code> require different fixtures, you can combine the previous techniques and
 * provide each test with just the fixture or fixtures it needs. Here's an example in which a <code>StringBuilder</code> and a
 * <code>ListBuffer</code> are provided via fixture traits, and file writer (that requires cleanup) is provided via the loan pattern:
 * </p>
 *
 * <pre class="stHighlight">
 * import java.io.FileWriter
 * import java.io.File
 * import collection.mutable.ListBuffer
 * import org.scalatest.FeatureSpec
 * 
 * class ExampleSpec extends FeatureSpec {
 * 
 *   final val tmpFile = "temp.txt"
 * 
 *   trait Builder {
 *     val builder = new StringBuilder("ScalaTest is ")
 *   }
 * 
 *   trait Buffer {
 *     val buffer = ListBuffer("ScalaTest", "is")
 *   }
 * 
 *   def withWriter(testCode: FileWriter => Any) {
 *     val writer = new FileWriter(tmpFile) // set up the fixture
 *     try {
 *       testCode(writer) // "loan" the fixture to the test
 *     }
 *     finally {
 *       writer.close() // clean up the fixture
 *     }
 *   }
 * 
 *   scenario("user is productive using the test framework") { // This test needs the StringBuilder fixture
 *     new Builder {
 *       builder.append("productive!")
 *       assert(builder.toString === "ScalaTest is productive!")
 *     }
 *   }
 * 
 *   scenario("tests are readable") { // This test needs the ListBuffer[String] fixture
 *     new Buffer {
 *       buffer += ("readable!")
 *       assert(buffer === List("ScalaTest", "is", "readable!"))
 *     }
 *   }
 * 
 *   scenario("the test framework is user-friendly") { // This test needs the FileWriter fixture
 *     withWriter { writer =>
 *       writer.write("Hello, user!")
 *       writer.flush()
 *       assert(new File(tmpFile).length === 12)
 *     }
 *   }
 * 
 *   scenario("test code is clear and concise") { // This test needs the StringBuilder and ListBuffer
 *     new Builder with Buffer {
 *       builder.append("clear!")
 *       buffer += ("concise!")
 *       assert(builder.toString === "ScalaTest is clear!")
 *       assert(buffer === List("ScalaTest", "is", "concise!"))
 *     }
 *   }
 * 
 *   scenario("user composes test artifacts") { // This test needs all three fixtures
 *     new Builder with Buffer {
 *       builder.append("clear!")
 *       buffer += ("concise!")
 *       assert(builder.toString === "ScalaTest is clear!")
 *       assert(buffer === List("ScalaTest", "is", "concise!"))
 *       withWriter { writer =>
 *         writer.write(builder.toString)
 *         writer.flush()
 *         assert(new File(tmpFile).length === 19)
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * In the previous example, <code>"user is productive using the test framework</code> uses only the <code>StringBuilder</code> fixture, so it just instantiates
 * a <code>new Builder</code>, whereas <code>tests are readable</code> uses only the <code>ListBuffer</code> fixture, so it just intantiates
 * a <code>new Buffer</code>. <code>the test framework is user-friendly</code> needs just the <code>FileWriter</code> fixture, so it invokes
 * <code>withWriter</code>, which prepares and passes a <code>FileWriter</code> to the test (and takes care of closing it afterwords).
 * </p>
 *
 * <p>
 * Two tests need multiple fixtures: <code>test code is clear and concise</code> needs both the <code>StringBuilder</code> and the
 * <code>ListBuffer</code>, so it instantiates a class that mixes in both fixture traits with <code>new Builder with Buffer</code>.
 * <code>user composes test artifacts</code> needs all three fixtures, so in addition to <code>new Builder with Buffer</code> it also invokes
 * <code>withWriter</code>, wrapping just the of the test code that needs the fixture.
 * </p>
 *
 * <p>
 * Note that in this case, the loan pattern is being implemented via the <code>withWriter</code> method that takes a function, not
 * by overriding <code>fixture.FeatureSpec</code>'s <code>withFixture(OneArgTest)</code> method. <code>fixture.FeatureSpec</code> makes the most sense
 * if all (or at least most) tests need the same fixture, whereas in this <code>Suite</code> only two tests need the
 * <code>FileWriter</code>.
 * </p>
 *
 * <p>
 * In the previous example, the <code>withWriter</code> method passed an object into
 * the tests. Passing fixture objects into tests is generally a good idea when possible, but sometimes a side affect is unavoidable.
 * For example, if you need to initialize a database running on a server across a network, your with-fixture 
 * method will likely have nothing to pass. In such cases, simply create a with-fixture method that takes a by-name parameter and
 * performs setup and cleanup via side effects, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * def withDataInDatabase(test: => Any) {
 *   // initialize the database across the network
 *   try {
 *     test // "loan" the initialized database to the test
 *   }
 *   finally {
 *     // clean up the database
 *   }
 * }
 * </pre>
 * 
 * <p>
 * You can then use it like:
 * </p>
 * 
 * <pre class="stHighlight">
 * scenario("user logs in") {
 *   withDataInDatabase {
 *     // test user logging in scenario
 *   }
 * }
 * </pre>
 * 
 * <a name="composingFixtures"></a><h2>Composing stackable fixture traits</h2>
 *
 * <p>
 * In larger projects, teams often end up with several different fixtures that test classes need in different combinations,
 * and possibly initialized (and cleaned up) in different orders. A good way to accomplish this in ScalaTest is to factor the individual
 * fixtures into traits that can be composed using the <em>stackable trait</em> pattern. This can be done, for example, by placing
 * <code>withFixture</code> methods in several traits, each of which call <code>super.withFixture</code>. Here's an example in
 * which the <code>StringBuilder</code> and <code>ListBuffer[String]</code> fixtures used in the previous examples have been
 * factored out into two <em>stackable fixture traits</em> named <code>Builder</code> and <code>Buffer</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import org.scalatest.AbstractSuite
 * import collection.mutable.ListBuffer
 * 
 * trait Builder extends AbstractSuite { this: Suite =>
 *
 *   val builder = new StringBuilder
 *
 *   abstract override def withFixture(test: NoArgTest) {
 *     builder.append("ScalaTest is ")
 *     try {
 *       super.withFixture(test) // To be stackable, must call super.withFixture
 *     }
 *     finally {
 *       builder.clear()
 *     }
 *   }
 * }
 *
 * trait Buffer extends AbstractSuite { this: Suite =>
 *
 *   val buffer = new ListBuffer[String]
 *
 *   abstract override def withFixture(test: NoArgTest) {
 *     try {
 *       super.withFixture(test) // To be stackable, must call super.withFixture
 *     }
 *     finally {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSpec extends FeatureSpec with Builder with Buffer {
 * 
 *   feature("Fixtures can be shared") {
 * 
 *     scenario("user learns how to share fixtures") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     scenario("user enjoys writing tests with shared fixtures") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *       buffer += "clear"
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * By mixing in both the <code>Builder</code> and <code>Buffer</code> traits, <code>ExampleSpec</code> gets both fixtures, which will be
 * initialized before each test and cleaned up after. The order the traits are mixed together determines the order of execution.
 * In this case, <code>Builder</code> is "super" to </code>Buffer</code>. If you wanted <code>Buffer</code> to be "super"
 * to <code>Builder</code>, you need only switch the order you mix them together, like this: 
 * </p>
 *
 * <pre class="stHighlight">
 * class Example2Spec extends FeatureSpec with Buffer with Builder
 * </pre>
 *
 * <p>
 * And if you only need one fixture you mix in only that trait:
 * </p>
 *
 * <pre class="stHighlight">
 * class Example3Spec extends FeatureSpec with Builder
 * </pre>
 *
 * <p>
 * Another way to create stackable fixture traits is by extending the <a href="BeforeAndAfterEach.html"><code>BeforeAndAfterEach</code></a>
 * and/or <a href="BeforeAndAfterAll.html"><code>BeforeAndAfterAll</code></a> traits.
 * <code>BeforeAndAfterEach</code> has a <code>beforeEach</code> method that will be run before each test (like JUnit's <code>setUp</code>),
 * and an <code>afterEach</code> method that will be run after (like JUnit's <code>tearDown</code>).
 * Similarly, <code>BeforeAndAfterAll</code> has a <code>beforeAll</code> method that will be run before all tests,
 * and an <code>afterAll</code> method that will be run after all tests. Here's what the previously shown example would look like if it
 * were rewritten to use the <code>BeforeAndAfterEach</code> methods instead of <code>withFixture</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FeatureSpec
 * import org.scalatest.BeforeAndAfterEach
 * import collection.mutable.ListBuffer
 * 
 * trait Builder extends BeforeAndAfterEach { this: Suite =>
 * 
 *   val builder = new StringBuilder
 * 
 *   override def beforeEach() {
 *     builder.append("ScalaTest is ")
 *     super.beforeEach() // To be stackable, must call super.beforeEach
 *   }
 * 
 *   override def afterEach() {
 *     try {
 *       super.afterEach() // To be stackable, must call super.afterEach
 *     }
 *     finally {
 *       builder.clear()
 *     }
 *   }
 * }
 * 
 * trait Buffer extends BeforeAndAfterEach { this: Suite =>
 * 
 *   val buffer = new ListBuffer[String]
 * 
 *   override def afterEach() {
 *     try {
 *       super.afterEach() // To be stackable, must call super.afterEach
 *     }
 *     finally {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSpec extends FeatureSpec with Builder with Buffer {
 * 
 *   feature("Fixtures can be shared") {
 * 
 *     scenario("user learns how to share fixtures") {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     scenario("user enjoys writing tests with shared fixtures") {
 *       builder.append("fun!")
 *       assert(builder.toString === "ScalaTest is fun!")
 *       assert(buffer.isEmpty)
 *       buffer += "clear"
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * To get the same ordering as <code>withFixture</code>, place your <code>super.beforeEach</code> call at the end of each
 * <code>beforeEach</code> method, and the <code>super.afterEach</code> call at the beginning of each <code>afterEach</code>
 * method, as shown in the previous example. It is a good idea to invoke <code>super.afterEach</code> in a <code>try</code>
 * block and perform cleanup in a <code>finally</code> clause, as shown in the previous example, because this ensures the
 * cleanup code is performed even if <code>super.afterAll</code> throws an exception.
 * </p>
 *
 * <p>
 * One difference to bear in mind between the before-and-after traits and the <code>withFixture</code> methods, is that if
 * a <code>withFixture</code> method completes abruptly with an exception, it is considered a failed test. By contrast, if any of the
 * methods on the before-and-after traits (<em>i.e.</em>, <code>before</code>  and <code>after</code> of <code>BeforeAndAfter</code>,
 * <code>beforeEach</code> and <code>afterEach</code> of <code>BeforeAndAfterEach</code>,
 * and <code>beforeAll</code> and <code>afterAll</code> of <code>BeforeAndAfterAll</code>) complete abruptly, it is considered a
 * failed suite, which will result in a <a href="events/SuiteAborted.html"><code>SuiteAborted</code></a> event.
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
 *   private val buf = new ListBuffer[T]
 *
 *   def push(o: T) {
 *     if (!full)
 *       buf.prepend(o)
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
 * trait FeatureSpecStackBehaviors { this: FeatureSpec with GivenWhenThen =&gt;
 * 
 *   def nonEmptyStack(createNonEmptyStack: =&gt; Stack[Int], lastItemAdded: Int) {
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
 *   def nonFullStack(createNonFullStack: =&gt; Stack[Int]) {
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
 * </pre>
 *
 * <p>
 * If you load these classes into the Scala interpreter (with scalatest's JAR file on the class path), and execute it,
 * you'll see:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new StackFeatureSpec).execute()
 * <span class="stGreen">Feature: A Stack is pushed and popped 
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
 *     Then push throws IllegalStateException</span> 
 * </pre>
 * 
 * <p>
 * One thing to keep in mind when using shared tests is that in ScalaTest, each test in a suite must have a unique name.
 * If you register the same tests repeatedly in the same suite, one problem you may encounter is an exception at runtime
 * complaining that multiple tests are being registered with the same test name.
 * In a <code>FeatureSpec</code> there is no nesting construct analogous to <code>FunSpec</code>'s <code>describe</code> clause.
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
 * @author Bill Venners
 */
@Style("org.scalatest.finders.FeatureSpecFinder")
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
   * Returns a <code>Documenter</code> that during test execution will forward strings passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>FeatureSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  implicit protected def markup: Documenter = atomicDocumenter.get

  /**
   * Register a test with the given spec text, optional tags, and test function value that takes no arguments.
   * An invocation of this method is called an &#8220;example.&#8221;
   *
   * This method will register the test for later execution via an invocation of one of the <code>execute</code>
   * methods. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>FeatureSpec</code> instance.
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
    registerTest(Resources("scenario", specText), testFun _, "scenarioCannotAppearInsideAnotherScenario", "FeatureSpec.scala", "scenario", 4, -2, None, None, testTags: _*)
  }

  /**
   * Register a test to ignore, which has the given spec text, optional tags, and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>FeatureSpec</code> instance.
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
    registerIgnoredTest(Resources("scenario", specText), testFun _, "ignoreCannotAppearInsideAScenario", "FeatureSpec.scala", "ignore", 4, -2, testTags: _*)
  }
  
  /**
   * Describe a &#8220;subject&#8221; being specified and tested by the passed function value. The
   * passed function value may contain more describers (defined with <code>describe</code>) and/or tests
   * (defined with <code>it</code>). This trait's implementation of this method will register the
   * description string and immediately invoke the passed function.
   */
  protected def feature(description: String)(fun: => Unit) {

    if (!currentBranchIsTrunk)
      throw new NotAllowedException(Resources("cantNestFeatureClauses"), getStackDepthFun("FeatureSpec.scala", "feature"))

    registerNestedBranch(description, None, fun, "featureCannotAppearInsideAScenario", "FeatureSpec.scala", "feature", 7, -2)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>FeatureSpec</code> belong, and values
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
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args) {

    def invokeWithFixture(theTest: TestLeaf) {
      val theConfigMap = args.configMap
      withFixture(
        new NoArgTest {
          def name = testName
          def apply() { theTest.testFun() }
          def configMap = theConfigMap
        }
      )
    }

    runTestImpl(thisSuite, testName, args, false, invokeWithFixture)
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
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected override def runTests(testName: Option[String], args: Args) {
    runTestsImpl(thisSuite, testName, args, info, false, runTest)
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
   * </pre>
   *
   * <p>
   * Invoking <code>testNames</code> on this <code>FeatureSpec</code> will yield a set that contains the following
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

  override def run(testName: Option[String], args: Args) {
    runImpl(thisSuite, testName, args, super.run)
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
  
  /**
   * Suite style name.
   */
  final override val styleName: String = "org.scalatest.FeatureSpec"
}
