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
package org.scalatest    

import tools.DistributedTestRunnerSuite
import tools.SortingReporter
import OneInstancePerTest.RunTheTestInThisInstance

/**
 * Trait that causes that the tests of any suite it is mixed into to be run in parallel if
 * a <code>Distributor</code> is passed to <code>runTests</code>.
 *
 * <p>
 * ScalaTest's normal approach for running suites of tests in parallel is to run different suites in parallel,
 * but the tests of any one suite sequentially. This approach should provide sufficient distribution of the work load
 * in most cases, but some suites may encapsulate multiple long-running tests. Such suites may dominate the execution
 * time of the run. If so, mixing in this trait into just those suites will allow their long-running tests to run in parallel with each
 * other, thereby helping to reduce the total time required to run an entire run.
 * </p>
 *
 * <p>
 * Because this trait extends <code>OneInstancePerTest</code>,
 * each test will be run its own instance of the suite's class. This trait overrides the 
 * <code>runTests</code> method. If no <code>Distributor</code> is passed to <code>runTests</code>, 
 * this trait's implementation simply invokes its supertrait <code>OneInstancePerTest</code>'s implementation
 * of <code>runTests</code>, which will run each test in its own instance sequentially. If a <code>Distributor</code>
 * is passed, however, this traits' implementation of <code>runTests</code> will, for each test, wrap a new instance of the
 * suite in a special <em>wrapper suite</em> that will invoke just that one test, and passes the wrapper suites to the <code>Distributor</code>.
 * The thread or entity that takes a wrapper suite from the <code>Distributor</code> will invoke <code>run</code>
 * on the wrapper suite, which will run just one test. In this way, different tests of a suite that mixes in
 * <code>ParallelTestExecution</code> will run in parallel.
 * </p>
 *
 * @author Bill Venners
 */
trait ParallelTestExecution extends OneInstancePerTest {

  this: Suite =>

  /**
   * Run the tests of this suite in parallel.
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>RunArgs</code> for this run
   *
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected abstract override def runTest(testName: String, args: RunArgs) {

    // TODO: Should we make this runTest final, so it has to be the last one mixed in? If there
    // are things that wouldn't work if this wasn't last, then I think we should go ahead and make
    // it final in 2.0

    // distributor
    //   None      call super, because no distributor
    //   Some      this would be the one where we need to actually run the test, ignore the distributor
    args.distributor match {
      // If there's no distributor, then just run sequentially, via the regular OneInstancePerTest
      // algorithm
      case None => super.runTest(testName, args)
      case Some(distribute) =>
        val cm = args.configMap
        if (cm.contains(RunTheTestInThisInstance))
          super.runTest(testName, args)
        else {
          val wrappedInstance =
            new DistributedTestRunnerSuite(
              newInstance,
              testName, 
              args
            )
          distribute(wrappedInstance, args.tracker.nextTracker)
        }
    }
  }
  
  private var sortingReporter: Option[SortingReporter] = None

  override abstract protected def runTests(testName: Option[String], args: RunArgs) {
    sortingReporter match {
      case Some(rep) => 
      case None =>
        sortingReporter = Some(new SortingReporter(args.reporter, testNames.size))
    }
    super.runTests(testName, args.copy(reporter = sortingReporter.getOrElse(args.reporter)))
    
    /*val runArgs = 
      if (args.configMap.contains(RunTheTestInThisInstance))
        args
      else
        args.copy(reporter = new SortingReporter(args.reporter, testNames.size))
    super.runTests(testName, runArgs)*/
  }

  override def newInstance: Suite with ParallelTestExecution = {
    val instance = getClass.newInstance.asInstanceOf[Suite with ParallelTestExecution]
    instance.sortingReporter = sortingReporter
    instance
  }
}
