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

import OneInstancePerTest.RunTestInNewInstance
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import tools.{SuiteSortingReporter, DistributedTestRunnerSuite, TestSortingReporter, Runner}
import org.scalatest.events.Event
import org.scalatest.tools.ConcurrentDistributor

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
trait ParallelTestExecution extends OneInstancePerTest { this: Suite =>
  
  protected abstract override def runTests(testName: Option[String], args: RunArgs) {
    val newArgs = 
      if (args.configMap.contains(RunTestInNewInstance))
        args
      else {
        // Create TestSortingReporter for this suite, when distributor is ConcurrentDistributor
        args.distributor match {
          case Some(distributor) =>
            distributor match {
              case concurrentDistributor: ConcurrentDistributor => 
                args.copy(reporter = concurrentDistributor.createTestSortingReporter(suiteId, sortingTimeout, testNames.size))
              case _ =>
                args
            }
          case None =>
            args
        }
      }
    super.runTests(testName, newArgs)
  }

  protected abstract override def runTest(testName: String, args: RunArgs) {

    if (args.configMap.contains(RunTestInNewInstance)) {
      // In initial instance, so wrap the test in a DistributedTestRunnerSuite and pass it to the Distributor.
      val oneInstance = newInstance
      args.distributor match {
        case Some(distribute) =>
          distribute match {
            case concurrentDistributor: ConcurrentDistributor => 
              val testSortingReporter = concurrentDistributor.getTestSortingReporter(suiteId)
              distribute(new DistributedTestRunnerSuite(oneInstance, testName, args, testSortingReporter), args.tracker.nextTracker)
          }
        case None =>
          oneInstance.run(Some(testName), args)
      }
    }
    else {// In test-specific (distributed) instance, so just run the test. (RTINI was
         // removed by OIPT's implementation of runTests.)
      args.distributor match {
        case None => 
          super.runTest(testName, args)
        case Some(distribute) => 
          distribute match {
            case concurrentDistributor: ConcurrentDistributor =>
              val testSortingReporter = concurrentDistributor.getTestSortingReporter(suiteId)              
              super.runTest(testName, args)
              testSortingReporter.completedTest(testName)
            case _ => 
              super.runTest(testName, args)
          }
      }
      
    }
  }

  // Narrow the type
  override def newInstance: Suite with ParallelTestExecution = {
    val instance = getClass.newInstance.asInstanceOf[Suite with ParallelTestExecution]
    instance
  }
  
  private class TestEventReporter(testSortingReporter: TestSortingReporter, testName: String) extends Reporter {
    def apply(event: Event) {
      testSortingReporter.apply(testName, event)
    }
  }
  
  abstract override def run(testName: Option[String], args: RunArgs) {
    val newArgs = testName match {
      case Some(testName) => 
        args.distributor match {
          case None => 
            args
          case Some(distribute) => 
            distribute match {
              case distribute: ConcurrentDistributor =>
                val testSortingReporter = distribute.getTestSortingReporter(suiteId)   
                val testReporter = new TestEventReporter(testSortingReporter, testName)
                args.copy(reporter = testReporter)
              case _ => 
                args
            }
        }
      case None =>
        args
    }
    super.run(testName, newArgs)
  }
  
  protected def sortingTimeout: Span = Runner.testSortingReporterTimeout
}
