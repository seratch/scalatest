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
import scala.collection.immutable.ListSet
import org.scalatest.Suite.autoTagClassAnnotations

/**
 * A sister trait to <code>org.scalatest.FunSuite</code> that can pass a fixture object into its tests.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use trait <code>fixture.FunSuite</code> in situations for which <a href="../FunSuite.html"><code>FunSuite</code></a>
 * would be a good choice, when all or most tests need the same fixture objects
 * that must be cleaned up afterwords. <em>Note: <code>fixture.FunSuite</code> is intended for use in special situations, with trait <code>FunSuite</code> used for general needs. For
 * more insight into where <code>fixture.FunSuite</code> fits in the big picture, see the <a href="../FunSuite.html#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a> subsection of the <a href="../FunSuite.html#sharedFixtures">Shared fixtures</a> section in the documentation for trait <code>FunSuite</code>.</em>
 * </td></tr></table>
 * 
 * <p>
 * Trait <code>fixture.FunSuite</code> behaves similarly to trait <code>org.scalatest.FunSuite</code>, except that tests may have a
 * fixture parameter. The type of the
 * fixture parameter is defined by the abstract <code>FixtureParam</code> type, which is declared as a member of this trait.
 * This trait also declares an abstract <code>withFixture</code> method. This <code>withFixture</code> method
 * takes a <code>OneArgTest</code>, which is a nested trait defined as a member of this trait.
 * <code>OneArgTest</code> has an <code>apply</code> method that takes a <code>FixtureParam</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This trait's <code>runTest</code> method delegates the actual running of each test to <code>withFixture(OneArgTest)</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture(OneArgTest)</code> method (abstract in this trait) is responsible
 * for creating the fixture argument and passing it to the test function.
 * </p>
 * 
 * <p>
 * Subclasses of this trait must, therefore, do three things differently from a plain old <code>org.scalatest.FunSuite</code>:
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
 * If the fixture you want to pass into your tests consists of multiple objects, you will need to combine
 * them into one object to use this trait. One good approach to passing multiple fixture objects is
 * to encapsulate them in a case class. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * case class F(file: File, writer: FileWriter)
 * type FixtureParam = F
 * </pre>
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
 * package org.scalatest.examples.funsuite.oneargtest
 * 
 * import org.scalatest.fixture
 * import java.io._
 * 
 * class ExampleSuite extends fixture.FunSuite {
 * 
 *   case class F(file: File, writer: FileWriter)
 *   type FixtureParam = F
 * 
 *   def withFixture(test: OneArgTest) {
 * 
 *     // create the fixture
 *     val file = File.createTempFile("hello", "world")
 *     val writer = new FileWriter(file)
 *     val theFixture = F(file, writer)
 * 
 *     try {
 *       writer.write("ScalaTest is ") // set up the fixture
 *       withFixture(test.toNoArgTest(theFixture)) // "loan" the fixture to the test
 *     }
 *     finally writer.close() // clean up the fixture
 *   }
 * 
 *   test("testing should be easy") { f =&gt;
 *     f.writer.write("easy!")
 *     f.writer.flush()
 *     assert(f.file.length === 18)
 *   }
 * 
 *   test("testing should be fun") { f =&gt;
 *     f.writer.write("fun!")
 *     f.writer.flush()
 *     assert(f.file.length === 17)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If a test fails, the <code>OneArgTest</code> function will complete abruptly with an exception describing the failure.
 * To ensure clean up happens even if a test fails, you should invoke the test function from inside a <code>try</code> block and do the cleanup in a
 * <code>finally</code> clause, as shown in the previous example.
 * </p>
 *
 * <a name="sharingFixturesAcrossClasses"></a><h2>Sharing fixtures across classes</h2>
 *
 * <p>
 * If multiple test classes need the same fixture, you can define the <code>FixtureParam</code> and <code>withFixture(OneArgTest)</code> implementations
 * in a trait, then mix that trait into the test classes that need it. For example, if your application requires a database and your integration tests
 * use that database, you will likely have many test classes that need a database fixture. You can create a "database fixture" trait that creates a
 * database with a unique name, passes the connector into the test, then removes the database once the test completes. This is shown in the following example:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.fixture.funsuite.sharing
 * 
 * import java.util.concurrent.ConcurrentHashMap
 * import org.scalatest.fixture
 * import DbServer._
 * import java.util.UUID.randomUUID
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
 * trait DbFixture { this: fixture.Suite =&gt;
 * 
 *   type FixtureParam = Db
 * 
 *   // Allow clients to populate the database after
 *   // it is created
 *   def populateDb(db: Db) {}
 * 
 *   def withFixture(test: OneArgTest) {
 *     val dbName = randomUUID.toString
 *     val db = createDb(dbName) // create the fixture
 *     try {
 *       populateDb(db) // setup the fixture
 *       withFixture(test.toNoArgTest(db)) // "loan" the fixture to the test
 *     }
 *     finally removeDb(dbName) // clean up the fixture
 *   }
 * }
 * 
 * class ExampleSuite extends fixture.FunSuite with DbFixture {
 * 
 *   override def populateDb(db: Db) { // setup the fixture
 *     db.append("ScalaTest is ")
 *   }
 * 
 *   test("testing should be easy") { db =&gt;
 *       db.append("easy!")
 *       assert(db.toString === "ScalaTest is easy!")
 *   }
 * 
 *   test("testing should be fun") { db =&gt;
 *       db.append("fun!")
 *       assert(db.toString === "ScalaTest is fun!")
 *   }
 * 
 *   // This test doesn't need a Db
 *   test("test code should be clear") { () =&gt;
 *       val buf = new StringBuffer
 *       buf.append("ScalaTest code is ")
 *       buf.append("clear!")
 *       assert(buf.toString === "ScalaTest code is clear!")
 *   }
 * }
 * </pre>
 *
 * <p>
 * Often when you create fixtures in a trait like <code>DbFixture</code>, you'll still need to enable individual test classes
 * to "setup" a newly created fixture before it gets passed into the tests. A good way to accomplish this is to pass the newly
 * created fixture into a setup method, like <code>populateDb</code> in the previous example, before passing it to the test
 * function. Classes that need to perform such setup can override the method, as does <code>ExampleSuite</code>.
 * </p>
 *
 * <p>
 * If a test doesn't need the fixture, you can indicate that by providing a no-arg instead of a one-arg function, as is done in the
 * third test in the previous example, &ldquo;<code>test code should be clear</code>&rdquo;. In other words, instead of starting your function literal
 * with something like &ldquo;<code>db =&gt;</code>&rdquo;, you'd start it with &ldquo;<code>() =&gt;</code>&rdquo;. For such tests, <code>runTest</code>
 * will not invoke <code>withFixture(OneArgTest)</code>. It will instead directly invoke <code>withFixture(NoArgTest)</code>.
 * </p>
 *
 *
 * <p>
 * Both examples shown above demonstrate the technique of giving each test its own "fixture sandbox" to play in. When your fixtures
 * involve external side-effects, like creating files or databases, it is a good idea to give each file or database a unique name as is
 * done in these examples. This keeps tests completely isolated, allowing you to run them in parallel if desired. You could mix
 * <code>ParallelTestExecution</code> into either of these <code>ExampleSuite</code> classes, and the tests would run in parallel just fine.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.FunSuiteFinder"))
trait FunSuite extends Suite { thisSuite =>

  private final val engine = new FixtureEngine[FixtureParam]("concurrentFixtureFunSuiteMod", "FixtureFunSuite")
  import engine._
  
  private[scalatest] val sourceFileName = "FunSuite.scala"

  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>fixture.FunSuite</code> is being executed, such as from inside a test function, it will forward the information to
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
  protected def test(testName: String, testTags: Tag*)(testFun: FixtureParam => Any) {
    registerTest(testName, testFun, "testCannotAppearInsideAnotherTest", sourceFileName, "test", 4, -2, None, None, None, testTags: _*)
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
  protected def ignore(testName: String, testTags: Tag*)(testFun: FixtureParam => Any) {
    registerIgnoredTest(testName, testFun, "ignoreCannotAppearInsideATest", sourceFileName, "ignore", 4, -2, None, testTags: _*)
  }

  /**
  * An immutable <code>Set</code> of test names. If this <code>fixture.FunSuite</code> contains no tests, this method returns an empty <code>Set</code>.
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
   * @param args the <code>Args</code> for this run
   *
   * @throws IllegalArgumentException if <code>testName</code> is defined but a test with that name does not exist on this <code>fixture.FunSuite</code>
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = {

    def invokeWithFixture(theTest: TestLeaf) {
      theTest.testFun match {
        case wrapper: NoArgTestWrapper[_] =>
          withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, args.configMap))
        case fun => withFixture(new TestFunAndConfigMap(testName, fun, args.configMap))
      }
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>fixture.FunSuite</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>fixture.FunSuite</code> contains no tags, this method returns an empty
   * <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to
   * methods <code>test</code> and <code>ignore</code>.
   * </p>
   * 
   * <p>
   * In addition, this trait's implementation will also auto-tag tests with class level annotations.  
   * For example, if you annotate @Ignore at the class level, all test methods in the class will be auto-annotated with @Ignore.
   * </p>
   */
  override def tags: Map[String, Set[String]] = autoTagClassAnnotations(atomic.get.tagsMap, this)

  protected override def runTests(testName: Option[String], args: Args): Status = {
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  override def run(testName: Option[String], args: Args): Status = {
    runImpl(thisSuite, testName, args: Args, super.run)
  }

  /**
   * Registers shared tests.
   *
   * <p>
   * This method enables the following syntax for shared tests in a <code>fixture.FunSuite</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * testsFor(nonEmptyStack(lastValuePushed))
   * </pre>
   *
   * <p>
   * This method just provides syntax sugar intended to make the intent of the code clearer.
   * Because the parameter passed to it is
   * type <code>Unit</code>, the expression will be evaluated before being passed, which
   * is sufficient to register the shared tests. For examples of shared tests, see the
   * <a href="../FunSuite.html#SharedTests">Shared tests section</a> in the main documentation for
   * trait <code>FunSuite</code>.
   * </p>
   */
  protected def testsFor(unit: Unit) {}

  /**
   * Implicitly converts a function that takes no parameters and results in <code>PendingNothing</code> to
   * a function from <code>FixtureParam</code> to <code>Any</code>, to enable pending tests to registered as by-name parameters
   * by methods that require a test function that takes a <code>FixtureParam</code>.
   *
   * <p>
   * This method makes it possible to write pending tests as simply <code>(pending)</code>, without needing
   * to write <code>(fixture => pending)</code>.
   * </p>
   */
  protected implicit def convertPendingToFixtureFunction(f: => PendingNothing): (FixtureParam => Any) = {
    fixture => f
  }

  /**
   * Implicitly converts a function that takes no parameters and results in <code>Any</code> to
   * a function from <code>FixtureParam</code> to <code>Any</code>, to enable no-arg tests to registered
   * by methods that require a test function that takes a <code>FixtureParam</code>.
   */
  protected implicit def convertNoArgToFixtureFunction(fun: () => Any): (FixtureParam => Any) =
    new NoArgTestWrapper(fun)
  
  /**
   * Suite style name.
   */
  final override val styleName: String = "org.scalatest.fixture.FunSuite"
    
  override def testDataFor(testName: String, theConfigMap: Map[String, Any] = Map.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
