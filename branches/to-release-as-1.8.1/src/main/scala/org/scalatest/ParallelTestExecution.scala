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
import OneInstancePerTest._

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

  // Skipping runTests here, but that's OK, because by mixing in ParallelTestExecution, the programmer decided
  // that the super.runTests should be replaced by the one defined in ParallelTestExecution.
  private[scalatest] def runOneTest(testName: String, reporter: Reporter, stopper: Stopper,
                         configMap: Map[String, Any], tracker: Tracker) {

    runTest(testName, reporter, stopper, configMap, tracker)
  }
  
  protected abstract override def runTest(testName: String, reporter: Reporter, stopper: Stopper, configMap: Map[String, Any], tracker: Tracker) {
    if (configMap.contains(RunTheTestInThisInstance)) {
      val filter = configMap(PassedInFilter).asInstanceOf[Filter]
      val distributor = configMap(PassedInDistributor).asInstanceOf[Option[Distributor]]
      val oneInstance = newInstance.asInstanceOf[ParallelTestExecution]
      distributor match {
        case None =>
          oneInstance.run(Some(testName), reporter, stopper, filter, configMap, distributor, tracker)
        case Some(distribute) => 
          distribute(new DistributedTestRunnerSuite(oneInstance, testName), tracker.nextTracker)
      }
    }
    else
      super.runTest(testName, reporter, stopper, configMap, tracker)
  }
}
