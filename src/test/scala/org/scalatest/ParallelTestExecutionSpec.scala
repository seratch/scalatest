package org.scalatest

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest.events.Event
import org.scalatest.events.ScopeOpened
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.ScopeClosed
import collection.mutable.ListBuffer
import org.scalatest.events.InfoProvided
import org.scalatest.tools.ConcurrentDistributor
import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import org.scalatest.tools.SuiteRunner
import org.scalatest.tools.SuiteSortingReporter
import org.scalatest.events.SuiteStarting
import org.scalatest.events.SuiteCompleted
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.Future

class ParallelTestExecutionSpec extends FunSpec with ShouldMatchers {
  /*
  Need 3 tests at least
  1. should have the events reported in correct order when tests are executed in parallel
     For that one, pass in a Distributor that runs with just one thread and orders things
     in a predefined, out of order order.

  2. DistributedSuiteSorter should wait for completedTests instead of moving on when it
     gets a SuiteCompleted.

  3. Both of these should time out. So we need a test for each that shows the timeout
     happened. I.e., it will move on when waiting for something.
   */
  private def checkScopeOpened(event: Event, message: String) {
    event match {
      case scopeOpened: ScopeOpened => assert(scopeOpened.message === message)
      case _ => fail("Expected ScopedOpened, but got " + event.getClass.getName)
    }
  }
  
  private def checkScopeClosed(event: Event, message: String) {
    event match {
      case scopeClosed: ScopeClosed => assert(scopeClosed.message === message)
      case _ => fail("Expected ScopedOpened, but got " + event.getClass.getName)
    }
  }
  
  private def checkTestStarting(event: Event, testName: String) {
    event match {
      case testStarting: TestStarting => assert(testStarting.testName === testName)
      case _ => fail("Expected TestStarting, but got " + event.getClass.getName)
    }
  }
  
  private def checkTestSucceeded(event: Event, testName: String) {
    event match {
      case testSucceeded: TestSucceeded => assert(testSucceeded.testName === testName)
      case _ => fail("Expected TestStarting, but got " + event.getClass.getName)
    }
  }
  
  private def checkInfoProvided(event: Event, message: String) {
    event match {
      case infoProvided: InfoProvided => assert(infoProvided.message === message)
      case _ => fail("Expected InfoProvided, but got " + event.getClass.getName)
    }
  }
  
  private def checkSuiteStarting(event: Event, suiteId: String) {
    event match {
      case suiteStarting: SuiteStarting => assert(suiteStarting.suiteId === suiteId)
      case _ => fail("Expected SuiteStarting, but got " + event.getClass.getName)
    }
  }
  
  private def checkSuiteCompleted(event: Event, suiteId: String) {
    event match {
      case suiteCompleted: SuiteCompleted => assert(suiteCompleted.suiteId === suiteId)
      case _ => fail("Expected SuiteCompleted, but got " + event.getClass.getName)
    }
  }
  
  describe("ParallelTestExecution") {

    class ControlledOrderDistributor extends Distributor {
      val buf = ListBuffer.empty[(Suite, RunArgs)]
      def apply(suite: Suite, args: RunArgs) {
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
    
    class ControlledOrderConcurrentDistributor(poolSize: Int) extends Distributor {
      private val futureQueue = new LinkedBlockingQueue[Future[T] forSome { type T }]
      
      val buf = ListBuffer.empty[SuiteRunner]
      val execSvc: ExecutorService = Executors.newFixedThreadPool(2)
      def apply(suite: Suite, args: RunArgs) {
        buf += new SuiteRunner(suite, args)
      }
      def executeInOrder() {
        for (suiteRunner <- buf) {
          val future: Future[_] = execSvc.submit(suiteRunner)
          futureQueue.put(future)
        }
        Thread.sleep(3000)
        //while (futureQueue.peek != null) 
          //futureQueue.poll().get()
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

    it("should have the events reported in correct order when tests are executed in parallel") {

      def withDistributor(fun: ControlledOrderDistributor => Unit) {

        val recordingReporter = new EventRecordingReporter
        val outOfOrderDistributor = new ControlledOrderDistributor
        val suiteSortingReporter = new SuiteSortingReporter(recordingReporter)
        val spec = new ExampleParallelSpec
        val tracker = new Tracker()
        suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, spec.suiteName, spec.suiteId, Some(spec.getClass.getName), None))
        spec.run(None, RunArgs(suiteSortingReporter, distributor = Some(outOfOrderDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
        suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, spec.suiteName, spec.suiteId, Some(spec.getClass.getName), None))
        
        fun(outOfOrderDistributor)

        val eventRecorded = recordingReporter.eventsReceived

        assert(eventRecorded.size === 18)
        
        checkSuiteStarting(eventRecorded(0), spec.suiteId)
        checkScopeOpened(eventRecorded(1), "Subject 1")
        checkTestStarting(eventRecorded(2), "Subject 1 should have behavior 1a")
        checkTestSucceeded(eventRecorded(3), "Subject 1 should have behavior 1a")
        checkTestStarting(eventRecorded(4), "Subject 1 should have behavior 1b")
        checkTestSucceeded(eventRecorded(5), "Subject 1 should have behavior 1b")
        checkTestStarting(eventRecorded(6), "Subject 1 should have behavior 1c")
        checkTestSucceeded(eventRecorded(7), "Subject 1 should have behavior 1c")
        checkScopeClosed(eventRecorded(8), "Subject 1")

        checkScopeOpened(eventRecorded(9), "Subject 2")
        checkTestStarting(eventRecorded(10), "Subject 2 should have behavior 2a")
        checkTestSucceeded(eventRecorded(11), "Subject 2 should have behavior 2a")
        checkTestStarting(eventRecorded(12), "Subject 2 should have behavior 2b")
        checkTestSucceeded(eventRecorded(13), "Subject 2 should have behavior 2b")
        checkTestStarting(eventRecorded(14), "Subject 2 should have behavior 2c")
        checkTestSucceeded(eventRecorded(15), "Subject 2 should have behavior 2c")
        checkScopeClosed(eventRecorded(16), "Subject 2")
        checkSuiteCompleted(eventRecorded(17), spec.suiteId)
      }
      withDistributor(_.executeInOrder())
      withDistributor(_.executeInReverseOrder())
    }
    
    it("should have InfoProvided fired from before and after block in correct order when tests are executed in parallel") {
      
      def withDistributor(fun: ControlledOrderDistributor => Unit) {

        val recordingReporter = new EventRecordingReporter
        val outOfOrderDistributor = new ControlledOrderDistributor
        val suiteSortingReporter = new SuiteSortingReporter(recordingReporter)
        val spec = new ExampleBeforeAfterParallelSpec
        val tracker = new Tracker()
        suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, spec.suiteName, spec.suiteId, Some(spec.getClass.getName), None))
        spec.run(None, RunArgs(suiteSortingReporter, distributor = Some(outOfOrderDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
        suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, spec.suiteName, spec.suiteId, Some(spec.getClass.getName), None))
        fun(outOfOrderDistributor)
        
        val eventRecorded = recordingReporter.eventsReceived
        assert(eventRecorded.size === 30)

        checkSuiteStarting(eventRecorded(0), spec.suiteId)
        checkScopeOpened(eventRecorded(1), "Thing 1")
        checkInfoProvided(eventRecorded(2), "In Before")
        checkTestStarting(eventRecorded(3), "Thing 1 do thing 1a")
        checkTestSucceeded(eventRecorded(4), "Thing 1 do thing 1a")
        checkInfoProvided(eventRecorded(5), "In After")
        checkInfoProvided(eventRecorded(6), "In Before")
        checkTestStarting(eventRecorded(7), "Thing 1 do thing 1b")
        checkTestSucceeded(eventRecorded(8), "Thing 1 do thing 1b")
        checkInfoProvided(eventRecorded(9), "In After")
        checkInfoProvided(eventRecorded(10), "In Before")
        checkTestStarting(eventRecorded(11), "Thing 1 do thing 1c")
        checkTestSucceeded(eventRecorded(12), "Thing 1 do thing 1c")
        checkInfoProvided(eventRecorded(13), "In After")
        checkScopeClosed(eventRecorded(14), "Thing 1")
        
        checkScopeOpened(eventRecorded(15), "Thing 2")
        checkInfoProvided(eventRecorded(16), "In Before")
        checkTestStarting(eventRecorded(17), "Thing 2 do thing 2a")
        checkTestSucceeded(eventRecorded(18), "Thing 2 do thing 2a")
        checkInfoProvided(eventRecorded(19), "In After")
        checkInfoProvided(eventRecorded(20), "In Before")
        checkTestStarting(eventRecorded(21), "Thing 2 do thing 2b")
        checkTestSucceeded(eventRecorded(22), "Thing 2 do thing 2b")
        checkInfoProvided(eventRecorded(23), "In After")
        checkInfoProvided(eventRecorded(24), "In Before")
        checkTestStarting(eventRecorded(25), "Thing 2 do thing 2c")
        checkTestSucceeded(eventRecorded(26), "Thing 2 do thing 2c")
        checkInfoProvided(eventRecorded(27), "In After")
        checkScopeClosed(eventRecorded(28), "Thing 2")
        checkSuiteCompleted(eventRecorded(29), spec.suiteId)
      }
      withDistributor(_.executeInOrder())
      withDistributor(_.executeInReverseOrder())
    }
    
    it("should have the blocking event fired without waiting when timeout reaches, and when the missing event finally reach later, it should just get fired") {
      def withDistributor(fun: ControlledOrderConcurrentDistributor => Unit) {
        val recordingReporter = new EventRecordingReporter
        val args = RunArgs(recordingReporter)
        val outOfOrderConcurrentDistributor = new ControlledOrderConcurrentDistributor(2)
        val suiteSortingReporter = new SuiteSortingReporter(recordingReporter)
        val spec = new ExampleTimeoutParallelSpec()
        val tracker = new Tracker()
        suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, spec.suiteName, spec.suiteId, Some(spec.getClass.getName), None))
        spec.run(None, RunArgs(suiteSortingReporter, distributor = Some(outOfOrderConcurrentDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
        suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, spec.suiteName, spec.suiteId, Some(spec.getClass.getName), None))
        
        fun(outOfOrderConcurrentDistributor)

        val eventRecorded = recordingReporter.eventsReceived
        assert(eventRecorded.size === 18)

        checkSuiteStarting(eventRecorded(0), spec.suiteId)
        checkScopeOpened(eventRecorded(1), "Thing 1")
        checkTestStarting(eventRecorded(2), "Thing 1 do thing 1a")
        checkTestSucceeded(eventRecorded(3), "Thing 1 do thing 1a")
        checkTestStarting(eventRecorded(4), "Thing 1 do thing 1b")        
        checkTestStarting(eventRecorded(5), "Thing 1 do thing 1c")
        checkTestSucceeded(eventRecorded(6), "Thing 1 do thing 1c")
        checkScopeClosed(eventRecorded(7), "Thing 1")
        
        checkScopeOpened(eventRecorded(8), "Thing 2")
        checkTestStarting(eventRecorded(9), "Thing 2 do thing 2a")
        checkTestSucceeded(eventRecorded(10), "Thing 2 do thing 2a")
        checkTestStarting(eventRecorded(11), "Thing 2 do thing 2b")
        checkTestSucceeded(eventRecorded(12), "Thing 2 do thing 2b")
        checkTestStarting(eventRecorded(13), "Thing 2 do thing 2c")
        checkTestSucceeded(eventRecorded(14), "Thing 2 do thing 2c")
        checkScopeClosed(eventRecorded(15), "Thing 2")
        // Now the missing one.
        checkTestSucceeded(eventRecorded(16), "Thing 1 do thing 1b")
        
        checkSuiteCompleted(eventRecorded(17), spec.suiteId)
      }

      withDistributor(_.executeInOrder())
      withDistributor(_.executeInReverseOrder())
    }
    
    it("should have the events reported in correct order when multiple suite's tests are executed in parallel") {
      def withDistributor(fun: ControlledOrderConcurrentDistributor => Unit) = {
        val recordingReporter = new EventRecordingReporter
        val outOfOrderConcurrentDistributor = new ControlledOrderConcurrentDistributor(2)
        val suiteSortingReporter = new SuiteSortingReporter(recordingReporter)
        val spec1 = new ExampleParallelSpec()
        val spec2 = new ExampleBeforeAfterParallelSpec()
        
        val tracker = new Tracker()
        suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, spec1.suiteName, spec1.suiteId, Some(spec1.getClass.getName), None))
        suiteSortingReporter(SuiteStarting(tracker.nextOrdinal, spec2.suiteName, spec2.suiteId, Some(spec2.getClass.getName), None))
        
        spec1.run(None, RunArgs(suiteSortingReporter, distributor = Some(outOfOrderConcurrentDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
        spec2.run(None, RunArgs(suiteSortingReporter, distributor = Some(outOfOrderConcurrentDistributor), distributedSuiteSorter = Some(suiteSortingReporter)))
        
        suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, spec1.suiteName, spec1.suiteId, Some(spec1.getClass.getName), None))
        suiteSortingReporter(SuiteCompleted(tracker.nextOrdinal, spec2.suiteName, spec2.suiteId, Some(spec2.getClass.getName), None))
        
        fun(outOfOrderConcurrentDistributor)
        
        recordingReporter.eventsReceived
      }
      
      val spec1SuiteId = new ExampleParallelSpec().suiteId
      val spec2SuiteId = new ExampleBeforeAfterParallelSpec().suiteId
      
      val inOrderEvents = withDistributor(_.executeInOrder)
      
      assert(inOrderEvents.size === 48)
      
      checkSuiteStarting(inOrderEvents(0), spec1SuiteId)
      checkScopeOpened(inOrderEvents(1), "Subject 1")
      checkTestStarting(inOrderEvents(2), "Subject 1 should have behavior 1a")
      checkTestSucceeded(inOrderEvents(3), "Subject 1 should have behavior 1a")
      checkTestStarting(inOrderEvents(4), "Subject 1 should have behavior 1b")
      checkTestSucceeded(inOrderEvents(5), "Subject 1 should have behavior 1b")
      checkTestStarting(inOrderEvents(6), "Subject 1 should have behavior 1c")
      checkTestSucceeded(inOrderEvents(7), "Subject 1 should have behavior 1c")
      checkScopeClosed(inOrderEvents(8), "Subject 1")

      checkScopeOpened(inOrderEvents(9), "Subject 2")
      checkTestStarting(inOrderEvents(10), "Subject 2 should have behavior 2a")
      checkTestSucceeded(inOrderEvents(11), "Subject 2 should have behavior 2a")
      checkTestStarting(inOrderEvents(12), "Subject 2 should have behavior 2b")
      checkTestSucceeded(inOrderEvents(13), "Subject 2 should have behavior 2b")
      checkTestStarting(inOrderEvents(14), "Subject 2 should have behavior 2c")
      checkTestSucceeded(inOrderEvents(15), "Subject 2 should have behavior 2c")
      checkScopeClosed(inOrderEvents(16), "Subject 2")
      checkSuiteCompleted(inOrderEvents(17), spec1SuiteId)
      
      checkSuiteStarting(inOrderEvents(18), spec2SuiteId)
      checkScopeOpened(inOrderEvents(19), "Thing 1")
      checkInfoProvided(inOrderEvents(20), "In Before")
      checkTestStarting(inOrderEvents(21), "Thing 1 do thing 1a")
      checkTestSucceeded(inOrderEvents(22), "Thing 1 do thing 1a")
      checkInfoProvided(inOrderEvents(23), "In After")
      checkInfoProvided(inOrderEvents(24), "In Before")
      checkTestStarting(inOrderEvents(25), "Thing 1 do thing 1b")
      checkTestSucceeded(inOrderEvents(26), "Thing 1 do thing 1b")
      checkInfoProvided(inOrderEvents(27), "In After")
      checkInfoProvided(inOrderEvents(28), "In Before")
      checkTestStarting(inOrderEvents(29), "Thing 1 do thing 1c")
      checkTestSucceeded(inOrderEvents(30), "Thing 1 do thing 1c")
      checkInfoProvided(inOrderEvents(31), "In After")
      checkScopeClosed(inOrderEvents(32), "Thing 1")
        
      checkScopeOpened(inOrderEvents(33), "Thing 2")
      checkInfoProvided(inOrderEvents(34), "In Before")
      checkTestStarting(inOrderEvents(35), "Thing 2 do thing 2a")
      checkTestSucceeded(inOrderEvents(36), "Thing 2 do thing 2a")
      checkInfoProvided(inOrderEvents(37), "In After")
      checkInfoProvided(inOrderEvents(38), "In Before")
      checkTestStarting(inOrderEvents(39), "Thing 2 do thing 2b")
      checkTestSucceeded(inOrderEvents(40), "Thing 2 do thing 2b")
      checkInfoProvided(inOrderEvents(41), "In After")
      checkInfoProvided(inOrderEvents(42), "In Before")
      checkTestStarting(inOrderEvents(43), "Thing 2 do thing 2c")
      checkTestSucceeded(inOrderEvents(44), "Thing 2 do thing 2c")
      checkInfoProvided(inOrderEvents(45), "In After")
      checkScopeClosed(inOrderEvents(46), "Thing 2")
      checkSuiteCompleted(inOrderEvents(47), spec2SuiteId)
      
      val reverseOrderEvents = withDistributor(_.executeInReverseOrder)
      
      assert(reverseOrderEvents.size === 48)
      
      checkSuiteStarting(reverseOrderEvents(0), spec1SuiteId)
      checkScopeOpened(reverseOrderEvents(1), "Subject 1")
      checkTestStarting(reverseOrderEvents(2), "Subject 1 should have behavior 1a")
      checkTestSucceeded(reverseOrderEvents(3), "Subject 1 should have behavior 1a")
      checkTestStarting(reverseOrderEvents(4), "Subject 1 should have behavior 1b")
      checkTestSucceeded(reverseOrderEvents(5), "Subject 1 should have behavior 1b")
      checkTestStarting(reverseOrderEvents(6), "Subject 1 should have behavior 1c")
      checkTestSucceeded(reverseOrderEvents(7), "Subject 1 should have behavior 1c")
      checkScopeClosed(reverseOrderEvents(8), "Subject 1")

      checkScopeOpened(reverseOrderEvents(9), "Subject 2")
      checkTestStarting(reverseOrderEvents(10), "Subject 2 should have behavior 2a")
      checkTestSucceeded(reverseOrderEvents(11), "Subject 2 should have behavior 2a")
      checkTestStarting(reverseOrderEvents(12), "Subject 2 should have behavior 2b")
      checkTestSucceeded(reverseOrderEvents(13), "Subject 2 should have behavior 2b")
      checkTestStarting(reverseOrderEvents(14), "Subject 2 should have behavior 2c")
      checkTestSucceeded(reverseOrderEvents(15), "Subject 2 should have behavior 2c")
      checkScopeClosed(reverseOrderEvents(16), "Subject 2")
      checkSuiteCompleted(reverseOrderEvents(17), spec1SuiteId)
      
      checkSuiteStarting(reverseOrderEvents(18), spec2SuiteId)
      checkScopeOpened(reverseOrderEvents(19), "Thing 1")
      checkInfoProvided(reverseOrderEvents(20), "In Before")
      checkTestStarting(reverseOrderEvents(21), "Thing 1 do thing 1a")
      checkTestSucceeded(reverseOrderEvents(22), "Thing 1 do thing 1a")
      checkInfoProvided(reverseOrderEvents(23), "In After")
      checkInfoProvided(reverseOrderEvents(24), "In Before")
      checkTestStarting(reverseOrderEvents(25), "Thing 1 do thing 1b")
      checkTestSucceeded(reverseOrderEvents(26), "Thing 1 do thing 1b")
      checkInfoProvided(reverseOrderEvents(27), "In After")
      checkInfoProvided(reverseOrderEvents(28), "In Before")
      checkTestStarting(reverseOrderEvents(29), "Thing 1 do thing 1c")
      checkTestSucceeded(reverseOrderEvents(30), "Thing 1 do thing 1c")
      checkInfoProvided(reverseOrderEvents(31), "In After")
      checkScopeClosed(reverseOrderEvents(32), "Thing 1")
        
      checkScopeOpened(reverseOrderEvents(33), "Thing 2")
      checkInfoProvided(reverseOrderEvents(34), "In Before")
      checkTestStarting(reverseOrderEvents(35), "Thing 2 do thing 2a")
      checkTestSucceeded(reverseOrderEvents(36), "Thing 2 do thing 2a")
      checkInfoProvided(reverseOrderEvents(37), "In After")
      checkInfoProvided(reverseOrderEvents(38), "In Before")
      checkTestStarting(reverseOrderEvents(39), "Thing 2 do thing 2b")
      checkTestSucceeded(reverseOrderEvents(40), "Thing 2 do thing 2b")
      checkInfoProvided(reverseOrderEvents(41), "In After")
      checkInfoProvided(reverseOrderEvents(42), "In Before")
      checkTestStarting(reverseOrderEvents(43), "Thing 2 do thing 2c")
      checkTestSucceeded(reverseOrderEvents(44), "Thing 2 do thing 2c")
      checkInfoProvided(reverseOrderEvents(45), "In After")
      checkScopeClosed(reverseOrderEvents(46), "Thing 2")
      checkSuiteCompleted(reverseOrderEvents(47), spec2SuiteId)
    }
  }
}