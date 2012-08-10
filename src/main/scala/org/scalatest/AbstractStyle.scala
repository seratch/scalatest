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

/**
 * Trait defining abstract "lifecycle" methods that are implemented in <code>Suite</code> and can
 * be overridden in stackable modification traits.
 *
 * <p>
 * The main purpose of <code>AbstractStyle</code> is to differentiate core <code>Suite</code>
 * traits, such as <code>Suite</code>, <code>FunSuite</code>, and <code>FunSpec</code> from stackable
 * modification traits for <code>Suite</code>s such as <code>BeforeAndAfterEach</code>, <code>OneInstancePerTest</code>,
 * and <code>SequentialNestedSuiteExecution</code>. Because these stackable traits extend <code>AbstractStyle</code>
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
trait AbstractStyle { this: Suite =>

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
  * Although subclass and subtrait implementations of this method may return a <code>Set</code> whose iterator produces <code>String</code>
  * test names in a well-defined order, the contract of this method does not required a defined order. Subclasses are free to
  * implement this method and return test names in either a defined or undefined order.
  * </p>
  */
  def testNames: Set[String]

  /**
  * An <code>IndexedSeq</code> of this <code>Suite</code> object's nested <code>Suite</code>s. If this <code>Suite</code> contains no nested <code>Suite</code>s,
  * this method returns an empty <code>IndexedSeq</code>.
  */
  def nestedSuites: IndexedSeq[Suite]

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names with which tests in this <code>Suite</code> are marked, and
   * whose values are the <code>Set</code> of test names marked with each tag.  If this <code>Suite</code> contains no tags, this
   * method returns an empty <code>Map</code>.
   *
   * <p>
   * Subclasses may implement this method to define and/or discover tags in a custom manner, but overriding method implementations
   * should never return an empty <code>Set</code> as a value. If a tag has no tests, its name should not appear as a key in the
   * returned <code>Map</code>.
   * </p>
   */
  def tags: Map[String, Set[String]]

  /**
   * The total number of tests that are expected to run when this <code>Suite</code>'s <code>run</code> method is invoked.
   *
   * @param filter a <code>Filter</code> with which to filter tests to count based on their tags
   */
  def expectedTestCount(filter: Filter): Int
  
  /**
   * The fully qualified name of the class that can be used to rerun this suite.
   */
  def rerunner: Option[String]
  
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

/**
 * <strong>AbstractSuite has been deprecated and will be removed in a future version of ScalaTest. Please change all occurances of
 * AbstractSuite to AbstractStyle. (This is just a name change.)</strong>
 */
@deprecated("Please use AbstractStyle instead")
trait AbstractSuite extends AbstractStyle { this: Suite => }

