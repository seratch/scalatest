/*
 * Copyright 2001-2012 Artima, Inc.
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

import Suite.getDecodedName
import Suite.getSimpleNameOfAnObjectsClass
import Suite.isTestMethodGoodies
import Suite.takesInformer
import Suite.simpleNameForTest
import Suite.takesCommunicator
import Suite.InformerInParens
import Suite.autoTagClassAnnotations
import Suite.testMethodTakesAnInformer
import tools.SuiteDiscoveryHelper
import java.lang.reflect.Method
import collection.immutable.TreeSet

/**
 * Trait defining abstract "lifecycle" methods that are implemented in <code>Suite</code> and can
 * be overridden in stackable modification traits.
 *
 * <p>
 * The main purpose of <code>AbstractSuite</code> is to differentiate core <code>Suite</code>
 * traits, such as <code>Suite</code>, <code>FunSuite</code>, and <code>FunSpec</code> from stackable
 * modification traits for <code>Suite</code>s such as <code>BeforeAndAfterEach</code>, <code>OneInstancePerTest</code>,
 * and <code>SequentialNestedSuiteExecution</code>. Because these stackable traits extend <code>AbstractSuite</code>
 * instead of <code>Suite</code>, you can't define a suite by simply extending one of the stackable traits:
 * </p>
 *
 * <pre class="stHighlight">
 * class MySuite extends BeforeAndAfterEach // Won't compile
 * </pre>
 *
 * <p>
 * Instead, you need to extend a core <code>Suite</code> trait and mix the stackable <code>BeforeAndAfterEach</code> trait
 * into that, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * class MySuite extends FunSuite with BeforeAndAfterEach // Compiles fine
 * </pre>
 *
 * @author Bill Venners
 */
trait AbstractSuite {

  /**
   * A string ID for this <code>AbstractSuite</code> that is intended to be unique among all suites reported during a run.
   *
   * <p>
   * This trait's
   * implementation of this method returns the fully qualified name of this object's class.
   * Each suite reported during a run will commonly be an instance of a different <code>AbstractSuite</code> class,
   * and in such cases, this default implementation of this method will suffice. However, in special cases
   * you may need to override this method to ensure it is unique for each reported suite. For example, if you write
   * a <code>AbstractSuite</code> subclass that reads in a file whose name is passed to its constructor and dynamically
   * creates a suite of tests based on the information in that file, you will likely need to override this method
   * in your <code>AbstractSuite</code> subclass, perhaps by appending the pathname of the file to the fully qualified class name.
   * That way if you run a suite of tests based on a directory full of these files, you'll have unique suite IDs for
   * each reported suite.
   * </p>
   *
   * <p>
   * The suite ID is <em>intended</em> to be unique, because ScalaTest does not enforce that it is unique. If it is not
   * unique, then you may not be able to uniquely identify a particular test of a particular suite. This ability is used,
   * for example, to dynamically tag tests as having failed in the previous run when rerunning only failed tests.
   * </p>
   *
   * @return this <code>Suite</code> object's ID.
   */
  def suiteId: String = this.getClass.getName

  /**
   * A user-friendly suite name for this <code>Suite</code>.
   *
   * <p>
   * This default
   * implementation of this method returns the simple name of this object's class. The
   * Suite trait's implementation of <code>runNestedSuites</code> calls this method to obtain a
   * name for <code>Report</code>s to pass to the <code>suiteStarting</code>, <code>suiteCompleted</code>,
   * and <code>suiteAborted</code> methods of the <code>Reporter</code>.
   * </p>
   *
   * @return this <code>Suite</code> object's suite name.
   */
  def suiteName = getSimpleNameOfAnObjectsClass(this)

  // Decoded suite name enclosed using backtick (`), currently for internal use only.
  private[scalatest] val decodedSuiteName:Option[String] = getDecodedName(suiteName)

  /**
   * A test function taking no arguments, which also provides a test name and config map.
   *
   * <p>
   * <code>Suite</code>'s implementation of <code>runTest</code> passes instances of this trait
   * to <code>withFixture</code> for every test method it executes. It invokes <code>withFixture</code>
   * for every test, including test methods that take an <code>Informer</code>. For the latter case,
   * the <code>Informer</code> to pass to the test method is already contained inside the
   * <code>NoArgTest</code> instance passed to <code>withFixture</code>.
   * </p>
   */
  protected trait NoArgTest extends (() => Unit) with TestData {

    /**
     * Runs the code of the test.
     */
    def apply()

  }

  /**
   * Runs the passed test function with a fixture established by this method.
   *
   * <p>
   * This method should set up the fixture needed by the tests of the
   * current suite, invoke the test function, and if needed, perform any clean
   * up needed after the test completes. Because the <code>NoArgTest</code> function
   * passed to this method takes no parameters, preparing the fixture will require
   * side effects, such as initializing an external database.
   * </p>
   *
   * @param test the no-arg test function to run with a fixture
   */
  protected def withFixture(test: NoArgTest)

  /**
   * Runs this suite of tests.
   *
   * @param testName an optional name of one test to execute. If <code>None</code>, all relevant tests should be executed.
   *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   */
  def run(testName: Option[String], args: Args)

  /**
   * Runs zero to many of this suite's nested suites.
   *
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if <code>args</code> is <code>null</code>.
   */
  protected def runNestedSuites(args: Args)

  /**
   * Runs zero to many of this suite's tests.
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if either <code>testName</code> or <code>args</code> is <code>null</code>.
   */
  protected def runTests(testName: Option[String], args: Args)

  /**
   * Runs a test.
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   *
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, <code>configMap</code>,
   *     or <code>tracker</code> is <code>null</code>.
   */
  protected def runTest(
    testName: String,
    args: Args
  )

  /**
   * A <code>Set</code> of test names. If this <code>Suite</code> contains no tests, this method returns an empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method uses Java reflection to discover all public methods whose name starts with <code>"test"</code>,
   * which take either nothing or a single <code>Informer</code> as parameters. For each discovered test method, it assigns a test name
   * comprised of just the method name if the method takes no parameters, or the method name plus <code>(Informer)</code> if the
   * method takes a <code>Informer</code>. Here are a few method signatures and the names that this trait's implementation assigns them:
   * </p>
   *
   * <pre class="stHighlight">
   * def testCat() {}         // test name: "testCat"
   * def testCat(Informer) {} // test name: "testCat(Informer)"
   * def testDog() {}         // test name: "testDog"
   * def testDog(Informer) {} // test name: "testDog(Informer)"
   * def test() {}            // test name: "test"
   * def test(Informer) {}    // test name: "test(Informer)"
   * </pre>
   *
   * <p>
   * This trait's implementation of this method returns an immutable <code>Set</code> of all such names, excluding the name
   * <code>testNames</code>. The iterator obtained by invoking <code>elements</code> on this
   * returned <code>Set</code> will produce the test names in their <em>natural order</em>, as determined by <code>String</code>'s
   * <code>compareTo</code> method.
   * </p>
   *
   * <p>
   * This trait's implementation of <code>runTests</code> invokes this method
   * and calls <code>runTest</code> for each test name in the order they appear in the returned <code>Set</code>'s iterator.
   * Although this trait's implementation of this method returns a <code>Set</code> whose iterator produces <code>String</code>
   * test names in a well-defined order, the contract of this method does not required a defined order. Subclasses are free to
   * override this method and return test names in an undefined order, or in a defined order that's different from <code>String</code>'s
   * natural order.
   * </p>
   *
   * <p>
   * Subclasses may override this method to produce test names in a custom manner. One potential reason to override <code>testNames</code> is
   * to run tests in a different order, for example, to ensure that tests that depend on other tests are run after those other tests.
   * Another potential reason to override is allow tests to be defined in a different manner, such as methods annotated <code>@Test</code> annotations
   * (as is done in <code>JUnitSuite</code> and <code>TestNGSuite</code>) or test functions registered during construction (as is
   * done in <code>FunSuite</code> and <code>FunSpec</code>).
   * </p>
   */
  def testNames: Set[String] = {

    def isTestMethod(m: Method) = {

      // Factored out to share code with fixture.Suite.testNames
      val (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags) = isTestMethodGoodies(m)

      isInstanceMethod && (firstFour == "test") && ((hasNoParams && !isTestNames && !isTestTags) || takesInformer(m) || takesCommunicator(m))
    }

    val testNameArray =
      for (m <- getClass.getMethods; if isTestMethod(m))
      yield if (takesInformer(m)) m.getName + InformerInParens else m.getName

    val result = TreeSet.empty[String](EncodedOrdering) ++ testNameArray
    if (result.size != testNameArray.length) {
      throw new NotAllowedException("Howdy", 0)
    }
    result
  }

  /*
 Old style method names will have (Informer) at the end still, but new ones will
 not. This method will find the one without a Rep if the same name is used
 with and without a Rep.
  */
  private[scalatest] def getMethodForTestName(testName: String) =
    try {
      getClass.getMethod(
        simpleNameForTest(testName),
        (if (testMethodTakesAnInformer(testName)) Array(classOf[Informer]) else new Array[Class[_]](0)): _*
      )
    }
    catch {
      case e: NoSuchMethodException =>
        // Try (Rep) on the end
        try {
          getClass.getMethod(simpleNameForTest(testName), classOf[Rep])
        }
        catch {
          case e: NoSuchMethodException =>
            throw new IllegalArgumentException(Resources("testNotFound", testName))
        }
      case e =>
        throw e
    }

  /**
  * An <code>IndexedSeq</code> of this <code>Suite</code> object's nested <code>Suite</code>s. If this <code>Suite</code> contains no nested <code>Suite</code>s,
  * this method returns an empty <code>IndexedSeq</code>.
  */
  def nestedSuites: IndexedSeq[AbstractSuite]

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names with which tests in this <code>Suite</code> are marked, and
   * whose values are the <code>Set</code> of test names marked with each tag.  If this <code>Suite</code> contains no tags, this
   * method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation of this method uses Java reflection to discover any Java annotations attached to its test methods. The
   * fully qualified name of each unique annotation that extends <code>TagAnnotation</code> is considered a tag. This trait's
   * implementation of this method, therefore, places one key/value pair into to the
   * <code>Map</code> for each unique tag annotation name discovered through reflection. The mapped value for each tag name key will contain
   * the test method name, as provided via the <code>testNames</code> method.
   * </p>
   *
   * <p>
   * In addition to test methods annotations, this trait's implementation will also auto-tag test methods with class level annotations.
   * For example, if you annotate @Ignore at the class level, all test methods in the class will be auto-annotated with @Ignore.
   * </p>
   *
   * <p>
   * Subclasses may override this method to define and/or discover tags in a custom manner, but overriding method implementations
   * should never return an empty <code>Set</code> as a value. If a tag has no tests, its name should not appear as a key in the
   * returned <code>Map</code>.
   * </p>
   */
  def tags: Map[String, Set[String]] = {
    def getTags(testName: String) =
      for {
        a <- getMethodForTestName(testName).getDeclaredAnnotations
        annotationClass = a.annotationType
        if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
      } yield annotationClass.getName

    val testNameSet = testNames

    val testTags = Map() ++
      (for (testName <- testNameSet; if !getTags(testName).isEmpty)
      yield testName -> (Set() ++ getTags(testName)))

    autoTagClassAnnotations(testTags, this)
  }

  /**
   * The total number of tests that are expected to run when this <code>Suite</code>'s <code>run</code> method is invoked.
   *
   * <p>
   * This default implementation of this method returns the sum of:
   * </p>
   *
   * <ul>
   * <li>the size of the <code>testNames</code> <code>List</code>, minus the number of tests marked as ignored and
   * any tests that are exluded by the passed <code>Filter</code></li>
   * <li>the sum of the values obtained by invoking
   *     <code>expectedTestCount</code> on every nested <code>Suite</code> contained in
   *     <code>nestedSuites</code></li>
   * </ul>
   *
   * @param filter a <code>Filter</code> with which to filter tests to count based on their tags
   */
  def expectedTestCount(filter: Filter): Int = {

    // [bv: here was another tricky refactor. How to increment a counter in a loop]
    def countNestedSuiteTests(nestedSuites: List[AbstractSuite], filter: Filter): Int =
      nestedSuites.toList match {
        case List() => 0
        case nestedSuite :: nestedSuites =>
          nestedSuite.expectedTestCount(filter) + countNestedSuiteTests(nestedSuites, filter)
      }

    filter.runnableTestCount(testNames, tags, suiteId) + countNestedSuiteTests(nestedSuites.toList, filter)
  }

  /**
   * The fully qualified class name of the rerunner to rerun this suite.  This default implementation will look at this.getClass and see if it is
   * either an accessible Suite, or it has a WrapWith annotation. If so, it returns the fully qualified class name wrapped in a Some,
   * or else it returns None.
   */
  def rerunner: Option[String] = {
    val suiteClass = getClass
    val isAccessible = SuiteDiscoveryHelper.isAccessibleSuite(suiteClass)
    val hasWrapWithAnnotation = suiteClass.getAnnotation(classOf[WrapWith]) != null
    if (isAccessible || hasWrapWithAnnotation)
      Some(suiteClass.getName)
    else
      None
  }
  
  /**
   * This suite's style name.
   *
   * <p>
   * This lifecycle method provides a string that is used to determine whether this suite object's
   * style is one of the <a href="tools/Runner$.html#specifyingChosenStyles">chosen styles</a> for
   * the project.
   * </p>
   */
  val styleName: String


  /**
   * <strong>This overloaded form of <code>run</code> has been deprecated and will be removed in a future
   * version of ScalaTest. Please use the <code>run</code> method that takes two parameters instead.</strong>
   *
   * <p>
   * This final implementation of this method constructs a <code>Args</code> instance from the passed
   * <code>reporter</code>, <code>stopper</code>, <code>filter</code>, <code>configMap</code>, <code>distributor</code>,
   * and <code>tracker</code>, and invokes the overloaded <code>run</code> method that takes two parameters,
   * passing in the specified <code>testName</code> and the newly constructed <code>Args</code>. This method
   * implementation enables existing code that called into the old <code>run</code> method to continue to work
   * during the deprecation cycle. Subclasses and subtraits that overrode this method, however, will need to
   * be changed to use the new two-parameter form instead.
   * </p>
   *
   * @param testName an optional name of one test to execute. If <code>None</code>, all relevant tests should be executed.
   *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>Suite</code>.
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param filter a <code>Filter</code> with which to filter tests based on their tags
   * @param configMap a <code>Map</code> of key-value pairs that can be used by the executing <code>Suite</code> of tests.
   * @param distributor an optional <code>Distributor</code>, into which to put nested <code>Suite</code>s to be executed
   *              by another entity, such as concurrently by a pool of threads. If <code>None</code>, nested <code>Suite</code>s will be executed sequentially.
   * @param tracker a <code>Tracker</code> tracking <code>Ordinal</code>s being fired by the current thread.
   *
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   */
  final def run(
    testName: Option[String],
    reporter: Reporter,
    stopper: Stopper,
    filter: Filter,
    configMap: Map[String, Any],
    distributor: Option[Distributor],
    tracker: Tracker
  ) {  // TODO: test that this grabs chosenStyles out of config map
    run(testName, Args(reporter, stopper, filter, configMap, distributor, tracker, Set.empty))
  } 
}
