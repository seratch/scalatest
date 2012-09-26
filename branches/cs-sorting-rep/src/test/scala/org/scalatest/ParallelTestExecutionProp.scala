package org.scalatest

import org.scalatest.prop.Tables
import scala.collection.mutable.ListBuffer
import org.scalatest.events.Event
import org.scalatest.prop.TableDrivenPropertyChecks
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.Future
import org.scalatest.tools.SuiteRunner
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import org.scalatest.tools.SuiteSortingReporter
import org.scalatest.time.Span
import org.scalatest.time.Seconds
import org.scalatest.events.SuiteStarting
import org.scalatest.events.SuiteCompleted
import org.scalatest.time.Millis
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import org.scalatest.events.TestSucceeded
import org.scalatest.tools.TestSortingReporter
import org.scalatest.concurrent.Eventually
import org.scalatest.tools.DistributedTestRunnerSuite

class ParallelTestExecutionProp extends FunSuite 
  with TableDrivenPropertyChecks with SharedHelpers with Eventually
  with ParallelTestExecutionOrderExamples 
  with ParallelTestExecutionInfoExamples 
  with ParallelTestExecutionTestTimeoutExamples
  with ParallelTestExecutionParallelSuiteExamples 
  with ParallelTestExecutionSuiteTimeoutExamples {
  
  class ControlledOrderDistributor extends Distributor {
    val buf = ListBuffer.empty[(Suite, Args)]
    def apply(suite: Suite, args: Args) {
      buf += ((suite, args))
    }
    def executeInOrder() {
      for ((suite, args) <- buf) {
        suite.run(None, args)
      }
    }
    def executeInReverseOrder() {
      for ((suite, args) <- buf.reverse) {
        suite.run(None, args)
      }
    }

    def apply(suite: Suite, tracker: Tracker) {
      throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
    }
  }
  
  class TestHoldingControlledOrderDistributor extends Distributor {
    val buf = ListBuffer.empty[(DistributedTestRunnerSuite, Args)]
    def apply(suite: Suite, args: Args) {
      suite match {
        case dtrs: DistributedTestRunnerSuite => 
          buf += ((dtrs, args))
        case _ => 
          throw new UnsupportedOperationException("TestHoldingControlledOrderDistributor takes only DistributedTestRunnerSuite!")
      }
      
    }
    def executeInOrder() {
      for ((suite, args) <- buf) {
        suite.run(None, args)
      }
    }
    def executeInReverseOrder() {
      for ((suite, args) <- buf.reverse) {
        suite.run(None, args)
      }
    }
    def fireHoldEvent() {
      for ((suite, args) <- buf) {
        suite.suite match {
          case tter: TestTimeoutExpectedResults => 
            tter.holdingReporter.fireHoldEvent()
          case other => 
            throw new UnsupportedOperationException("Expected TestTimeoutExpectedResults type, but we got: " + other.getClass.getName)
        }
        
      }
    }
    def apply(suite: Suite, tracker: Tracker) {
      throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
    }
  }
  
  class ControlledOrderConcurrentDistributor(poolSize: Int) extends Distributor {
      private val futureQueue = new LinkedBlockingQueue[Future[T] forSome { type T }]
      
      val buf = ListBuffer.empty[SuiteRunner]
      val execSvc: ExecutorService = Executors.newFixedThreadPool(poolSize)
      def apply(suite: Suite, args: Args) {
        buf += new SuiteRunner(suite, args)
      }
      def executeInOrder() {
        for (suiteRunner <- buf) {
          val future: Future[_] = execSvc.submit(suiteRunner)
          futureQueue.put(future)
        }
        while (futureQueue.peek != null) 
          futureQueue.poll().get()
      }
      def executeInReverseOrder() {
        for (suiteRunner <- buf.reverse) {
          val future: Future[_] = execSvc.submit(suiteRunner)
          futureQueue.put(future)
        }
        while (futureQueue.peek != null)
          futureQueue.poll().get()
      }

      def apply(suite: Suite, tracker: Tracker) {
        throw new UnsupportedOperationException("Hey, we're not supposed to be calling this anymore!")
      }
    }
  
  def withDistributor(suite: Suite, fun: ControlledOrderDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val outOfOrderDistributor = new ControlledOrderDistributor
    suite.run(None, Args(recordingReporter, distributor = Some(outOfOrderDistributor)))
    fun(outOfOrderDistributor)

    recordingReporter.eventsReceived
  }
  
  def withDistributor(suite: Suite with TestTimeoutExpectedResults, fun: TestHoldingControlledOrderDistributor => Unit, holdUntilEventCount: Int, sortingTimeout: Span) = {
    val recordingReporter = new EventRecordingReporter
    val distributor = new TestHoldingControlledOrderDistributor
    suite.run(None, Args(recordingReporter, distributor = Some(distributor)))
    fun(distributor)
    eventually(timeout(sortingTimeout.scaledBy(3.0))) { 
      assert(recordingReporter.eventsReceived.size === holdUntilEventCount) 
    }
    distributor.fireHoldEvent()
    recordingReporter.eventsReceived
  }
  
  def withConcurrentDistributor(suite: Suite, fun: ControlledOrderConcurrentDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val args = Args(recordingReporter)
    val outOfOrderConcurrentDistributor = new ControlledOrderConcurrentDistributor(2)
    suite.run(None, Args(recordingReporter, distributor = Some(outOfOrderConcurrentDistributor)))
    fun(outOfOrderConcurrentDistributor)

    recordingReporter.eventsReceived
  }
  
  def withConcurrentDistributor(suite1: Suite, suite2: Suite, timeout: Span, fun: ControlledOrderConcurrentDistributor => Unit) = {
    val recordingReporter = new EventRecordingReporter
    val outOfOrderConcurrentDistributor = new ControlledOrderConcurrentDistributor(2)
    val suiteSortingReporter = new SuiteSortingReporter(recordingReporter, timeout, new PrintStream(new ByteArrayOutputStream))
    
    val tracker = new Tracker()
    suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
        
    suite1.run(None, Args(suiteSortingReporter, distributor = Some(outOfOrderConcurrentDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
    suite2.run(None, Args(suiteSortingReporter, distributor = Some(outOfOrderConcurrentDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
        
    suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, suite1.suiteName, suite1.suiteId, Some(suite1.getClass.getName), None))
    suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, suite2.suiteName, suite2.suiteId, Some(suite2.getClass.getName), None))
        
    fun(outOfOrderConcurrentDistributor)
        
    recordingReporter.eventsReceived
  }
  
  test("ParallelTestExecution should have the events reported in correct order when tests are executed in parallel") {
    forAll(orderExamples) { example =>
      val inOrderEvents = withDistributor(example, _.executeInOrder)
      example.assertOrderTest(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder)
      example.assertOrderTest(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have InfoProvided fired from before and after block in correct order when tests are executed in parallel") {
    forAll(infoExamples) { example =>
      val inOrderEvents = withDistributor(example, _.executeInOrder)
      example.assertBeforeAfterInfo(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder)
      example.assertBeforeAfterInfo(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have the blocking test's events fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired") {
    forAll(testTimeoutExamples) { example => 
      val inOrderEvents = withDistributor(example, _.executeInOrder, example.holdUntilEventCount, example.sortingTimeout)
      example.assertTestTimeoutTest(inOrderEvents)
      val reverseOrderEvents = withDistributor(example, _.executeInReverseOrder, example.holdUntilEventCount, example.sortingTimeout)
      example.assertTestTimeoutTest(reverseOrderEvents)
      /*val inOrderEvents = withConcurrentDistributor(example, _.executeInOrder)
      example.assertTestTimeoutTest(inOrderEvents)
      val reverseOrderEvents = withConcurrentDistributor(example, _.executeInReverseOrder)
      example.assertTestTimeoutTest(reverseOrderEvents)*/
    }
  }
  
  test("ParallelTestExecution should have the events reported in correct order when multiple suite's tests are executed in parallel") {
    forAll(parallelExamples) { example => 
      val inOrderEvents = withConcurrentDistributor(example.suite1, example.suite2, Span(5, Seconds), _.executeInOrder)
      example.assertParallelSuites(inOrderEvents)
      val reverseOrderEvents = withConcurrentDistributor(example.suite1, example.suite2, Span(5, Seconds), _.executeInReverseOrder)
      example.assertParallelSuites(reverseOrderEvents)
    }
  }
  
  test("ParallelTestExecution should have the blocking suite's events fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired") {
    forAll(suiteTimeoutExamples) { example =>
      val events = withConcurrentDistributor(example.suite1, example.suite2, Span(100, Millis), _.executeInOrder)
      example.assertSuiteTimeoutTest(events)
    }
  }
}