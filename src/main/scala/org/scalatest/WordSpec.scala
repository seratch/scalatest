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

import verb.{CanVerb, ResultOfAfterWordApplication, ShouldVerb, BehaveWord,
  MustVerb, StringVerbBlockRegistration}
import NodeFamily._
import scala.collection.immutable.ListSet
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepth
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import Suite.anErrorThatShouldCauseAnAbort

/**
 * Class that facilitates a &#8220;behavior-driven&#8221; style of development (BDD), in which tests
 * are combined with text that specifies the behavior the tests verify.
 * (In BDD, the word <em>example</em> is usually used instead of <em>test</em>. The word test will not appear
 * in your code if you use <code>WordSpec</code>, so if you prefer the word <em>example</em> you can use it. However, in this documentation
 * the word <em>test</em> will be used, for clarity and to be consistent with the rest of ScalaTest.)
 * Class <code>WordSpec</code> is so named because
 * you specification text is structured by placing words after strings.
 * Here's an example <code>WordSpec</code>:
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 * import scala.collection.mutable.Stack
 *
 * class StackSpec extends WordSpec {
 *
 *   "A Stack" should {
 *
 *     "pop values in last-in-first-out order" in {
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       assert(stack.pop() === 2)
 *       assert(stack.pop() === 1)
 *     }
 *
 *     "throw NoSuchElementException if an empty stack is popped" in {
 *       val emptyStack = new Stack[String]
 *       intercept[NoSuchElementException] {
 *         emptyStack.pop()
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * <em>Note: Class <code>WordSpec</code> is in part inspired by class <code>org.specs.Specification</code>, designed by
 * Eric Torreborre for the <a href="http://code.google.com/p/specs/" target="_blank">specs framework</a>.</em>
 * </p>
 *
 * <p>
 * See also: <a href="http://www.scalatest.org/getting_started_with_word_spec" target="_blank">Getting started with <code>WordSpec</code>.</a>
 * </p>
 *
 * <p>
 * In a <code>WordSpec</code> you write a one (or more) sentence specification for each bit of behavior you wish to
 * specify and test. Each specification sentence has a
 * "subject," which is sometimes called the <em>system under test</em> (or SUT). The 
 * subject is entity being specified and tested and also serves as the subject of the sentences you write for each test. A subject
 * can be followed by one of three verbs, <code>should</code>, <code>must</code>, or <code>can</code>, and a block. Here are some
 * examples:
 * </p>
 * 
 * <pre class="stHighlight">
 * "A Stack" should {
 *   // ...
 * }
 * "An Account" must {
 *   // ...
 * }
 * "A ShippingManifest" can {
 *   // ...
 * }
 * </pre>
 * 
 * <p>
 * You can describe a subject in varying situations by using a <code>when</code> clause. A <code>when</code> clause
 * follows the subject and precedes a block. In the block after the <code>when</code>, you place strings that describe a situation or a state
 * the subject may be in using a string, each followed by a verb. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack" when {
 *   "empty" should {
 *     // ...
 *   }
 *   "non-empty" should {
 *     // ...
 *   }
 *   "full" should {
 *     // ...
 *   }
 * }
 * </pre>
 * 
 * <p>
 * When you are ready to finish a sentence, you write a string followed by <code>in</code> and a block that
 * contains the code of the test. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 * 
 * class StackSpec extends WordSpec {
 *   "A Stack" when {
 *     "empty" should {
 *       "be empty" in {
 *         // ...
 *       }
 *       "complain on peek" in {
 *         // ...
 *       }
 *       "complain on pop" in {
 *         // ...
 *       }
 *     }
 *     "full" should {
 *       "be full" in {
 *         // ...
 *       }
 *       "complain on push" in {
 *         // ...
 *       }
 *     }
 *   }
 * }
 * </pre>
 * 
 * <p>
 * Running the above <code>StackSpec</code> in the interpreter would yield:
 * </p>
 * 
 * <pre class="stREPL">
 * <span class="stGreen">scala> (new StackSpec).execute()
 * StackSpec:
 * A Stack
 *   when empty
 * &nbsp; - should be empty
 * &nbsp; - should complain on peek
 * &nbsp; - should complain on pop
 * &nbsp; when full
 * &nbsp; - should be full
 * &nbsp; - should complain on push</span>
 * </pre>
 *
 * <p>
 * Note that the output does not exactly match the input in an effort to maximize readability.
 * Although the <code>WordSpec</code> code is nested, which can help you eliminate any repeated phrases
 * in the specification portion of your code, the output printed moves <code>when</code> and <code>should</code>
 * down to the beginning of the next line.
 * </p>
 *
 * <p>
 * Sometimes you may wish to eliminate repeated phrases inside the block following a <code>verb</code>. Here's an example
 * in which the phrase "provide an and/or operator, which" is repeated:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 * 
 * class AndOrSpec extends WordSpec {
 * 
 *   "The ScalaTest Matchers DSL" should {
 *     "provide an and operator, which returns silently when evaluating true and true" in {}
 *     "provide an and operator, which throws a TestFailedException when evaluating true and false" in {}
 *     "provide an and operator, which throws a TestFailedException when evaluating false and true" in {}
 *     "provide an and operator, which throws a TestFailedException when evaluating false and false" in {}
 *     "provide an or operator, which returns silently when evaluating true or true" in {}
 *     "provide an or operator, which returns silently when evaluating true or false" in {}
 *     "provide an or operator, which returns silently when evaluating false or true" in {}
 *     "provide an or operator, which throws a TestFailedException when evaluating false or false" in {}
 *   }
 * }
 * </pre>
 *
 * <p>
 * In such situations you can place <code>which</code> clauses inside the verb clause, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 * 
 * class AndOrSpec extends WordSpec {
 *
 *   "The ScalaTest Matchers DSL" should {
 *     "provide an and operator," which {
 *       "returns silently when evaluating true and true" in {}
 *       "throws a TestFailedException when evaluating true and false" in {}
 *       "throws a TestFailedException when evaluating false and true" in {}
 *       "throws a TestFailedException when evaluating false and false" in {}
 *     }
 *     "provide an or operator," which {
 *       "returns silently when evaluating true or true" in {}
 *       "returns silently when evaluating true or false" in {}
 *       "returns silently when evaluating false or true" in {}
 *       "throws a TestFailedException when evaluating false or false" in {}
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running the above <code>AndOrSpec</code> in the interpreter would yield:
 * </p>
 * 
 * <pre class="stREPL">
 * scala> (new AndOrSpec).execute()
 * <span class="stGreen">AndOrSpec:
 * The ScalaTest Matchers DSL
 *   should provide an and operator, which
 * &nbsp; - returns silently when evaluating true and true
 * &nbsp; - throws a TestFailedException when evaluating true and false
 * &nbsp; - throws a TestFailedException when evaluating false and true
 * &nbsp; - throws a TestFailedException when evaluating false and false
 * &nbsp; should provide an or operator, which
 * &nbsp; - returns silently when evaluating true or true
 * &nbsp; - returns silently when evaluating true or false
 * &nbsp; - returns silently when evaluating false or true
 * &nbsp; - throws a TestFailedException when evaluating false or false</span>
 * </pre>
 * 
 * <p>
 * Note that unlike <code>when</code> and <code>should</code>/<code>must</code>/<code>can</code>, a <code>which</code> appears
 * in the output right where you put it in the input, at the end of the line, to maximize readability.
 * </p>
 *
 * <p>
 * <a name="AfterWords">If</a> a word or phrase is repeated at the beginning of each string contained in a block, you can eliminate
 * that repetition by using an <em>after word</em>. An after word is a word or phrase that you can place
 * after <code>when</code>, a verb, or
 * <code>which</code>. For example, in the previous <code>WordSpec</code>, the word "provide" is repeated
 * at the beginning of each string inside the <code>should</code> block. You can factor out this duplication
 * like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 * 
 * class AndOrSpec extends WordSpec {
 * 
 *    def provide = afterWord("provide")
 * 
 *   "The ScalaTest Matchers DSL" should provide {
 *     "an and operator," which {
 *       "returns silently when evaluating true and true" in {}
 *       "throws a TestFailedException when evaluating true and false" in {}
 *       "that throws a TestFailedException when evaluating false and true" in {}
 *       "throws a TestFailedException when evaluating false and false" in {}
 *     }
 *     "an or operator," which {
 *       "returns silently when evaluating true or true" in {}
 *       "returns silently when evaluating true or false" in {}
 *       "returns silently when evaluating false or true" in {}
 *       "throws a TestFailedException when evaluating false or false" in {}
 *     }
 *   }
 * }
 * </pre>
 * 
 *  <p>
 *  Running the above version of <code>AndOrSpec</code> with the <code>provide</code> after word in the interpreter would give you:
 *  </p>
 * 
 * <pre class="stREPL">
 * scala> (new AndOrSpec).execute()
 * <span class="stGreen">AndOrSpec:
 * The ScalaTest Matchers DSL
 *   should provide
 *     an and operator, which
 * &nbsp;   - returns silently when evaluating true and true
 * &nbsp;   - throws a TestFailedException when evaluating true and false
 * &nbsp;   - that throws a TestFailedException when evaluating false and true
 * &nbsp;   - throws a TestFailedException when evaluating false and false
 * &nbsp;   an or operator, which
 * &nbsp;   - returns silently when evaluating true or true
 * &nbsp;   - returns silently when evaluating true or false
 * &nbsp;   - returns silently when evaluating false or true
 * &nbsp;   - throws a TestFailedException when evaluating false or false</span>
 * </pre>
 *
 * <p>
 * Once you've defined an after word, you can place it after <code>when</code>, a verb
 * (<code>should</code>, <code>must</code>, or <code>can</code>), or
 * <code>which</code>. (You can't place one after <code>in</code> or <code>is</code>, the
 * words that introduce a test.) Here's an example that has after words used in all three
 * places:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 * 
 * class ScalaTestGUISpec extends WordSpec {
 * 
 *   def theUser = afterWord("the user")
 *   def display = afterWord("display")
 *   def is = afterWord("is")
 * 
 *   "The ScalaTest GUI" when theUser {
 *     "clicks on an event report in the list box" should display {
 *       "a blue background in the clicked-on row in the list box" in {}
 *       "the details for the event in the details area" in {}
 *       "a rerun button," which is {
 *         "enabled if the clicked-on event is rerunnable" in {}
 *         "disabled if the clicked-on event is not rerunnable" in {}
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Running the previous <code>WordSpec</code> in the Scala interpreter would yield:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new ScalaTestGUISpec).execute()
 * <span class="stGreen">ScalaTestGUISpec:
 * The ScalaTest GUI
 *   when the user clicks on an event report in the list box
 *     should display
 * &nbsp;   - a blue background in the clicked-on row in the list box
 * &nbsp;   - the details for the event in the details area
 * &nbsp;     a rerun button, which is
 * &nbsp;     - enabled if the clicked-on event is rerunnable
 * &nbsp;     - disabled if the clicked-on event is not rerunnable</span>
 * </pre>
 *
 * <p>
 * A <code>WordSpec</code>'s lifecycle has two phases: the <em>registration</em> phase and the
 * <em>ready</em> phase. It starts in registration phase and enters ready phase the first time
 * <code>run</code> is called on it. It then remains in ready phase for the remainder of its lifetime.
 * </p>
 *
 * <p>
 * Tests can only be registered while the <code>WordSpec</code> is
 * in its registration phase. Any attempt to register a test after the <code>WordSpec</code> has
 * entered its ready phase, <em>i.e.</em>, after <code>run</code> has been invoked on the <code>WordSpec</code>,
 * will be met with a thrown <code>TestRegistrationClosedException</code>. The recommended style
 * of using <code>WordSpec</code> is to register tests during object construction as is done in all
 * the examples shown here. If you keep to the recommended style, you should never see a
 * <code>TestRegistrationClosedException</code>.
 * </p>
 *
 * <h2>Ignored tests</h2>
 *
 * To support the common use case of &#8220;temporarily&#8221; disabling a test, with the
 * good intention of resurrecting the test at a later time, <code>WordSpec</code> adds a method
 * <code>ignore</code> to strings that can be used instead of <code>in</code> to register a test. For example, to temporarily
 * disable the test with the name <code>"A Stack should pop values in last-in-first-out order"</code>, just
 * change &#8220;<code>in</code>&#8221; into &#8220;<code>ignore</code>,&#8221; like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 * import scala.collection.mutable.Stack
 *
 * class StackSpec extends WordSpec {
 *
 *   "A Stack" should {
 *
 *     "pop values in last-in-first-out order" ignore {
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       assert(stack.pop() === 2)
 *       assert(stack.pop() === 1)
 *     }
 *
 *     "throw NoSuchElementException if an empty stack is popped" in {
 *       val emptyStack = new Stack[String]
 *       intercept[NoSuchElementException] {
 *         emptyStack.pop()
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>StackSpec</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new StackSpec).execute()
 * </pre>
 *
 * <p>
 * It will run only the second test and report that the first test was ignored:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">StackSpec:
 * A Stack</span>
 * <span class="stYellow">- should pop values in last-in-first-out order !!! IGNORED !!!</span>
 * <span class="stGreen">- should throw NoSuchElementException if an empty stack is popped</span>
 * </pre>
 *
 * <h2>Informers</h2>
 *
 * <p>
 * One of the parameters to the <code>run</code> method is a <code>Reporter</code>, which
 * will collect and report information about the running suite of tests.
 * Information about suites and tests that were run, whether tests succeeded or failed, 
 * and tests that were ignored will be passed to the <code>Reporter</code> as the suite runs.
 * Most often the reporting done by default by <code>WordSpec</code>'s methods will be sufficient, but
 * occasionally you may wish to provide custom information to the <code>Reporter</code> from a test.
 * For this purpose, an <code>Informer</code> that will forward information to the current <code>Reporter</code>
 * is provided via the <code>info</code> parameterless method.
 * You can pass the extra information to the <code>Informer</code> via its <code>apply</code> method.
 * The <code>Informer</code> will then pass the information to the <code>Reporter</code> via an <code>InfoProvided</code> event.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 *
 * class ArithmeticSpec extends WordSpec {
 *
 *  "The Scala language" should {
 *     "add correctly" in {
 *       val sum = 2 + 3
 *       assert(sum === 5)
 *       info("addition seems to work")
 *     }
 *
 *     "subtract correctly" in {
 *       val diff = 7 - 2
 *       assert(diff === 5)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this <code>WordSpec</code> from the interpreter, you will see the following message
 * included in the printed report:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new ArithmeticSpec).execute()
 * <span class="stGreen">ArithmeticSpec:
 * The Scala language 
 * - should add correctly
 *   + addition seems to work 
 * - should subtract correctly</span>
 * </pre>
 *
 * <p>
 * One use case for the <code>Informer</code> is to pass more information about a specification to the reporter. For example,
 * the <code>GivenWhenThen</code> trait provides methods that use the implicit <code>info</code> provided by <code>WordSpec</code>
 * to pass such information to the reporter. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 * import org.scalatest.GivenWhenThen
 * 
 * class ArithmeticSpec extends WordSpec with GivenWhenThen {
 * 
 *  "The Scala language" should {
 * 
 *     "add correctly" in { 
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
 *     "subtract correctly" in {
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
 * </pre>
 *
 * <p>
 * If you run this <code>WordSpec</code> from the interpreter, you will see the following messages
 * included in the printed report:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new ArithmeticSpec).execute()
 * <span class="stGreen">ArithmeticSpec:
 * The Scala language 
 * - should add correctly
 *   + Given two integers 
 *   + When they are added 
 *   + Then the result is the sum of the two numbers 
 * - should subtract correctly
 *   + Given two integers 
 *   + When one is subtracted from the other 
 *   + Then the result is the difference of the two numbers</span> 
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
 * the actual test, and possibly the functionality it is intended to test, has not yet been implemented.
 * You can mark tests as pending in a <code>WordSpec</code> like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 *
 * class ArithmeticSpec extends WordSpec {
 *
 *   // Sharing fixture objects via instance variables
 *   val shared = 5
 *
 *  "The Scala language" should {
 *     "add correctly" in {
 *       val sum = 2 + 3
 *       assert(sum === shared)
 *     }
 *
 *     "subtract correctly" is (pending)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>ArithmeticSpec</code> with:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new ArithmeticSpec).execute()
 * </pre>
 *
 * <p>
 * It will run both tests but report that <code>The Scala language should subtract correctly</code> is pending. You'll see:
 * </p>
 *
 * <pre class="stREPL">
 * <span class="stGreen">The Scala language
 * - should add correctly</span>
 * <span class="stYellow">- should subtract correctly (pending)</span>
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
 * that used the <code>GivenWhenThen</code> trait. For example, the following snippet in a <code>WordSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 *  "The Scala language" should {
 *     "add correctly" in { 
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
 * <span class="stGreen">The Scala language</span>
 * <span class="stYellow">- should add correctly (pending)
 *   + Given two integers 
 *   + When they are added 
 *   + Then the result is the sum of the two numbers</span> 
 * </pre>
 *
 * <h2>Tagging tests</h2>
 *
 * A <code>WordSpec</code>'s tests may be classified into groups by <em>tagging</em> them with string names.
 * As with any suite, when executing a <code>WordSpec</code>, groups of tests can
 * optionally be included and/or excluded. To tag a <code>WordSpec</code>'s tests,
 * you pass objects that extend abstract class <code>org.scalatest.Tag</code> to <code>taggedAs</code> method
 * invoked on the string that describes the test you want to tag. Class <code>Tag</code> takes one parameter,
 * a string name.  If you have
 * created Java annotation interfaces for use as group names in direct subclasses of <code>org.scalatest.Suite</code>,
 * then you will probably want to use group names on your <code>WordSpec</code>s that match. To do so, simply 
 * pass the fully qualified names of the Java interfaces to the <code>Tag</code> constructor. For example, if you've
 * defined Java annotation interfaces with fully qualified names, <code>com.mycompany.tags.SlowTest</code> and <code>com.mycompany.tags.DbTest</code>, then you could
 * create matching groups for <code>WordSpec</code>s like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.Tag
 *
 * object SlowTest extends Tag("com.mycompany.tags.SlowTest")
 * object DbTest extends Tag("com.mycompany.tags.DbTest")
 * </pre>
 *
 * <p>
 * Given these definitions, you could place <code>WordSpec</code> tests into groups like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.WordSpec
 *
 * class ExampleSpec extends WordSpec {
 *
 *   "The Scala language" should {
 *
 *     "add correctly" taggedAs(SlowTest) in {
 *       val sum = 1 + 1
 *       assert(sum === 2)
 *     }
 *
 *     "subtract correctly" taggedAs(SlowTest, DbTest) in {
 *       val diff = 4 - 1
 *       assert(diff === 3)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * This code marks both tests with the <code>com.mycompany.tags.SlowTest</code> tag, 
 * and test <code>"The Scala language should subtract correctly"</code> with the <code>com.mycompany.tags.DbTest</code> tag.
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
 * import org.scalatest.WordSpec
 * import collection.mutable.ListBuffer
 *
 * class ExampleSpec extends WordSpec {
 * 
 *   def fixture =
 *     new {
 *       val builder = new StringBuilder("ScalaTest is ")
 *       val buffer = new ListBuffer[String]
 *     }
 * 
 *   "Testing" should {
 *
 *     "be easy" in {
 *       val f = fixture
 *       f.builder.append("easy!")
 *       assert(f.builder.toString === "ScalaTest is easy!")
 *       assert(f.buffer.isEmpty)
 *       f.buffer += "sweet"
 *     }
 * 
 *     "be fun" in {
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
 * import org.scalatest.WordSpec
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends WordSpec {
 * 
 *   trait Fixture {
 *     val builder = new StringBuilder("ScalaTest is ")
 *     val buffer = new ListBuffer[String]
 *   }
 * 
 *   "Testing" should {
 *
 *     "be easy" in {
 *       new Fixture {
 *         builder.append("easy!")
 *         assert(builder.toString === "ScalaTest is easy!")
 *         assert(buffer.isEmpty)
 *         buffer += "sweet"
 *       }
 *     }
 * 
 *     "be fun" in {
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
 * import org.scalatest.WordSpec
 * import org.scalatest.OneInstancePerTest
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends WordSpec with OneInstancePerTest {
 * 
 *   val builder = new StringBuilder("ScalaTest is ")
 *   val buffer = new ListBuffer[String]
 * 
 *   "Testing" should {
 *
 *     "be easy" in {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     "be fun" in {
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
 * import org.scalatest.WordSpec
 * import org.scalatest.BeforeAndAfter
 * import collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends WordSpec with BeforeAndAfter {
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
 *   "Testing" should {
 *
 *     "be easy" in {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     "be fun" in {
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
 * <code>runTest</code>, which is inherited by this class, passes a no-arg test function to <code>withFixture</code>. It is <code>withFixture</code>'s
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
 * import org.scalatest.WordSpec
 * import collection.mutable.ListBuffer
 *
 * class ExampleSpec extends WordSpec {
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
 *   "Testing" should {
 *
 *     "be easy" in {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 *
 *     "be fun" in {
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
 * To use the loan pattern, you can extend <code>WordSpec</code> (from the <code>org.scalatest.fixture</code> package) instead of
 * <code>WordSpec</code>. Each test in a <code>WordSpec</code> takes a fixture as a parameter, allowing you to pass the fixture into
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
 * class ExampleSpec extends fixture.WordSpec {
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
 *   "Testing" should {
 *
 *     "be easy" in { writer =>
 *       writer.write("Hello, test!")
 *       writer.flush()
 *       assert(new File(tmpFile).length === 12)
 *     }
 * 
 *     "be fun" in { writer =>
 *       writer.write("Hi, test!")
 *       writer.flush()
 *       assert(new File(tmpFile).length === 9)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * For more information, see the <a href="fixture/WordSpec.html">documentation for <code>org.scalatest.fixture.WordSpec</code></a>.
 * </p>
 *
 * <a name="differentFixtures"></a><h2>Providing different fixtures to different tests</h2>
 * 
 * <p>
 * If different tests in the same <code>WordSpec</code> require different fixtures, you can combine the previous techniques and
 * provide each test with just the fixture or fixtures it needs. Here's an example in which a <code>StringBuilder</code> and a
 * <code>ListBuffer</code> are provided via fixture traits, and file writer (that requires cleanup) is provided via the loan pattern:
 * </p>
 *
 * <pre class="stHighlight">
 * import java.io.FileWriter
 * import java.io.File
 * import collection.mutable.ListBuffer
 * import org.scalatest.WordSpec
 * 
 * class ExampleSpec extends WordSpec {
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
 *   "Testing" should {
 *
 *     "be productive" in { // This test needs the StringBuilder fixture
 *       new Builder {
 *         builder.append("productive!")
 *         assert(builder.toString === "ScalaTest is productive!")
 *       }
 *     }
 * 
 *     "be readable" in { // This test needs the ListBuffer[String] fixture
 *       new Buffer {
 *         buffer += ("readable!")
 *         assert(buffer === List("ScalaTest", "is", "readable!"))
 *       }
 *     }
 * 
 *     "be user-friendly" in { // This test needs the FileWriter fixture
 *       withWriter { writer =>
 *         writer.write("Hello, user!")
 *         writer.flush()
 *         assert(new File(tmpFile).length === 12)
 *       }
 *     }
 * 
 *     "be clear and concise" in { // This test needs the StringBuilder and ListBuffer
 *       new Builder with Buffer {
 *         builder.append("clear!")
 *         buffer += ("concise!")
 *         assert(builder.toString === "ScalaTest is clear!")
 *         assert(buffer === List("ScalaTest", "is", "concise!"))
 *       }
 *     }
 * 
 *     "be composable" in { // This test needs all three fixtures
 *       new Builder with Buffer {
 *         builder.append("clear!")
 *         buffer += ("concise!")
 *         assert(builder.toString === "ScalaTest is clear!")
 *         assert(buffer === List("ScalaTest", "is", "concise!"))
 *         withWriter { writer =>
 *           writer.write(builder.toString)
 *           writer.flush()
 *           assert(new File(tmpFile).length === 19)
 *         }
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * In the previous example, <code>be productive</code> uses only the <code>StringBuilder</code> fixture, so it just instantiates
 * a <code>new Builder</code>, whereas <code>be readable</code> uses only the <code>ListBuffer</code> fixture, so it just intantiates
 * a <code>new Buffer</code>. <code>be friendly</code> needs just the <code>FileWriter</code> fixture, so it invokes
 * <code>withWriter</code>, which prepares and passes a <code>FileWriter</code> to the test (and takes care of closing it afterwords).
 * </p>
 *
 * <p>
 * Two tests need multiple fixtures: <code>be clear and concise</code> needs both the <code>StringBuilder</code> and the
 * <code>ListBuffer</code>, so it instantiates a class that mixes in both fixture traits with <code>new Builder with Buffer</code>.
 * <code>be composable</code> needs all three fixtures, so in addition to <code>new Builder with Buffer</code> it also invokes
 * <code>withWriter</code>, wrapping just the of the test code that needs the fixture.
 * </p>
 *
 * <p>
 * Note that in this case, the loan pattern is being implemented via the <code>withWriter</code> method that takes a function, not
 * by overriding <code>WordSpec</code>'s <code>withFixture(OneArgTest)</code> method. <code>WordSpec</code> makes the most sense
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
 * "A user" can {
 *   "log onto the system" in {
 *     withDataInDatabase {
 *       // test user logging in scenario
 *     }
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
 * import org.scalatest.WordSpec
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
 * class ExampleSpec extends WordSpec with Builder with Buffer {
 * 
 *   "Testing" should {
 *
 *     "be easy" in {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     "be fun" in {
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
 * class Example2Spec extends WordSpec with Buffer with Builder
 * </pre>
 *
 * <p>
 * And if you only need one fixture you mix in only that trait:
 * </p>
 *
 * <pre class="stHighlight">
 * class Example3Spec extends WordSpec with Builder
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
 * import org.scalatest.WordSpec
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
 * class ExampleSpec extends WordSpec with Builder with Buffer {
 * 
 *   "Testing" should {
 *
 *     "be easy" in {
 *       builder.append("easy!")
 *       assert(builder.toString === "ScalaTest is easy!")
 *       assert(buffer.isEmpty)
 *       buffer += "sweet"
 *     }
 * 
 *     "be fun" in {
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
 * <a name="SharedTests"></a><h2>Shared tests</h2>
 *
 * <p>
 * Sometimes you may want to run the same test code on different fixture objects. In other words, you may want to write tests that are "shared"
 * by different fixture objects.  To accomplish this in a <code>WordSpec</code>, you first place shared tests in <em>behavior functions</em>.
 * These behavior functions will be invoked during the construction phase of any <code>WordSpec</code> that uses them, so that the tests they
 * contain will be registered as tests in that <code>WordSpec</code>.  For example, given this stack class:
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
 * <em>etc</em>. You may find you have several tests that make sense any time the stack is non-empty. Thus you'd ideally want to run
 * those same tests for three stack fixture objects: a full stack, a stack with a one item, and a stack with one item less than
 * capacity. With shared tests, you can factor these tests out into a behavior function, into which you pass the
 * stack fixture to use when running the tests. So in your <code>WordSpec</code> for stack, you'd invoke the
 * behavior function three times, passing in each of the three stack fixtures so that the shared tests are run for all three fixtures. You
 * can define a behavior function that encapsulates these shared tests inside the <code>WordSpec</code> that uses them. If they are shared
 * between different <code>WordSpec</code>s, however, you could also define them in a separate trait that is mixed into each <code>WordSpec</code>
 * that uses them.
 * </p>
 *
 * <p>
 * <a name="StackBehaviors">For</a> example, here the <code>nonEmptyStack</code> behavior function (in this case, a behavior <em>method</em>) is
 * defined in a trait along with another method containing shared tests for non-full stacks:
 * </p>
 * 
 * <pre class="stHighlight">
 * trait StackBehaviors { this: WordSpec =&gt;
 * 
 *   def nonEmptyStack(newStack: =&gt; Stack[Int], lastItemAdded: Int) {
 *
 *     "be non-empty" in {
 *       assert(!newStack.empty)
 *     }
 *
 *     "return the top item on peek" in {
 *       assert(newStack.peek === lastItemAdded)
 *     }
 *
 *     "not remove the top item on peek" in {
 *       val stack = newStack
 *       val size = stack.size
 *       assert(stack.peek === lastItemAdded)
 *       assert(stack.size === size)
 *     }
 *
 *     "remove the top item on pop" in {
 *       val stack = newStack
 *       val size = stack.size
 *       assert(stack.pop === lastItemAdded)
 *       assert(stack.size === size - 1)
 *     }
 *   }
 *
 *   def nonFullStack(newStack: =&gt; Stack[Int]) {
 *
 *     "not be full" in {
 *       assert(!newStack.full)
 *     }
 *
 *     "add to the top on push" in {
 *       val stack = newStack
 *       val size = stack.size
 *       stack.push(7)
 *       assert(stack.size === size + 1)
 *       assert(stack.peek === 7)
 *     }
 *   }
 * }
 * </pre>
 *
 *
 * <p>
 * Given these behavior functions, you could invoke them directly, but <code>WordSpec</code> offers a DSL for the purpose,
 * which looks like this:
 * </p>
 *
 * <pre class="stHighlight">
 * behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 * behave like nonFullStack(stackWithOneItem)
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
 * behave like nonEmptyStack // assuming lastValuePushed is also in scope inside nonEmptyStack
 * behave like nonFullStack
 * </pre>
 *
 * <p>
 * The recommended style, however, is the functional, pass-all-the-needed-values-in style. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * class SharedTestExampleSpec extends WordSpec with StackBehaviors {
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
 *   "A Stack" when {
 *     "empty" should {
 *       "be empty" in {
 *         assert(emptyStack.empty)
 *       }
 * 
 *       "complain on peek" in {
 *         intercept[IllegalStateException] {
 *           emptyStack.peek
 *         }
 *       }
 *
 *       "complain on pop" in {
 *         intercept[IllegalStateException] {
 *           emptyStack.pop
 *         }
 *       }
 *     }
 * 
 *     "it contains one item" should {
 *       behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 *       behave like nonFullStack(stackWithOneItem)
 *     }
 *     
 *     "it contains one item less than capacity" should {
 *       behave like nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed)
 *       behave like nonFullStack(stackWithOneItemLessThanCapacity)
 *     }
 * 
 *     "full" should {
 *       "be full" in {
 *         assert(fullStack.full)
 *       }
 * 
 *       behave like nonEmptyStack(fullStack, lastValuePushed)
 * 
 *       "complain on a push" in {
 *         intercept[IllegalStateException] {
 *           fullStack.push(10)
 *         }
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
 * scala> (new SharedTestExampleSpec).execute()
 * <span class="stGreen">SharedTestExampleSpec:
 * A Stack
 *   when empty
 * &nbsp; - should be empty
 * &nbsp; - should complain on peek
 * &nbsp; - should complain on pop
 * &nbsp; when it contains one item
 * &nbsp; - should be non-empty
 * &nbsp; - should return the top item on peek
 * &nbsp; - should not remove the top item on peek
 * &nbsp; - should remove the top item on pop
 * &nbsp; - should not be full
 * &nbsp; - should add to the top on push
 * &nbsp; when it contains one item less than capacity
 * &nbsp; - should be non-empty
 * &nbsp; - should return the top item on peek
 * &nbsp; - should not remove the top item on peek
 * &nbsp; - should remove the top item on pop
 * &nbsp; - should not be full
 * &nbsp; - should add to the top on push
 * &nbsp; when full
 * &nbsp; - should be full
 * &nbsp; - should be non-empty
 * &nbsp; - should return the top item on peek
 * &nbsp; - should not remove the top item on peek
 * &nbsp; - should remove the top item on pop
 * &nbsp; - should complain on a push</span>
 * </pre>
 * 
 * <p>
 * One thing to keep in mind when using shared tests is that in ScalaTest, each test in a suite must have a unique name.
 * If you register the same tests repeatedly in the same suite, one problem you may encounter is an exception at runtime
 * complaining that multiple tests are being registered with the same test name. A good way to solve this problem in a <code>WordSpec</code> is to make sure
 * each invocation of a behavior function is in the context of a different surrounding <code>when</code>, 
 * <code>should</code>/<code>must</code>/<code>can</code>, or <code>which</code> clause, because a test's name is the concatenation of its
 * surrounding clauses and after words, followed by the "spec text".
 * For example, the following code in a <code>WordSpec</code> would register a test with the name <code>"A Stack when empty should be empty"</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack" when {
 *   "empty" should {
 *     "be empty" in {
 *       assert(emptyStack.empty)
 *     }
 *   }
 * }
 * // ...
 * </pre>
 *
 * <p>
 * If the <code>"be empty"</code> test was factored out into a behavior function, it could be called repeatedly so long
 * as each invocation of the behavior function is in the context of a different surrounding <code>when</code> clauses.
 * </p>
 *
 * @author Bill Venners
 */
class WordSpec extends WordSpecLike
