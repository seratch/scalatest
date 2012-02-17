package org.scalatest.path

import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers
import org.scalatest.Stopper
import org.scalatest.Filter
import org.scalatest.Tracker
import org.scalatest.ParallelTestExecution
import org.scalatest.Distributor
import org.scalatest.tools.ConcurrentDistributor
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import org.scalatest.DispatchReporter
import org.scalatest.PathEngine

class FunSpecSpec extends org.scalatest.FunSpec with ShouldMatchers with SharedHelpers with PrivateMethodTester {

  describe("FunSpec ThreadLocal variable") {
    it("should be set by setPath and clear by getPath") {
      val setPath = PrivateMethod[Unit]('setPath)
      val getPath = PrivateMethod[Option[List[Int]]]('getPath)
      
      PathEngine invokePrivate setPath(List(1, 2, 3))
      PathEngine invokePrivate getPath() should be (Some(List(1, 2, 3)))
      PathEngine invokePrivate getPath() should be (None)
    }
  }
  
  class MyFunSpec extends org.scalatest.path.FunSpec with ShouldMatchers {
    
    import scala.collection.mutable.ListBuffer
    import MyFunSpec._

    var begin = 0
    var begin0 = 0
    var begin00 = 0
    var begin000 = 0
    var begin01 = 0
    var begin010 = 0
        
    var end = 0
    var end0 = 0
    var end00 = 0
    var end000 = 0
    var end01 = 0
    var end010 = 0

    instanceCount += 1 
    begin += 1
    
    describe("An empty list buffer") {

      begin0 += 1
      val buf = ListBuffer[Int]() 
      
      describe("when 1 is inserted") {

        begin00 += 1
        buf += 1 

        it("should have only 1 in it") {
          begin000 += 1
          buf should be (ListBuffer(1)) 
          firstTestCount += 1
          assert(begin === 1)
          assert(begin0 === 1)
          assert(begin00 === 1)
          assert(begin000 === 1)
          assert(begin01 === 0)
          assert(begin010 === 0)
          assert(end === 0)
          assert(end0 === 0)
          assert(end00 === 0)
          assert(end000 === 0)
          assert(end01 === 0)
          assert(end010 === 0)
          end000 += 1
        }
        firstDescCount += 1
        end00 += 1
      }
      
      describe("when 2 is inserted") {
        begin01 += 1
        buf += 2
        it("should have only 2 in it") {
          begin010 += 1
          buf should be (ListBuffer(2))
          secondTestCount += 1
          assert(begin === 1)
          assert(begin0 === 1)
          assert(begin00 === 0)
          assert(begin000 === 0)
          assert(begin01 === 1)
          assert(begin010 === 1)
          assert(end === 0)
          assert(end0 === 0)
          assert(end00 === 0)
          assert(end000 === 0)
          assert(end01 === 0)
          assert(end010 === 0)
          end010 += 1
        }
        secondDescCount += 1
        end01 += 1
      }
      outerDescCount += 1
      end0 += 1
    }
    end += 1
    if (end000 == 1) {
      assert(begin === 1)
      assert(begin0 === 1)
      assert(begin00 === 1)
      assert(begin000 === 1)
      assert(begin01 === 0)
      assert(begin010 === 0)
      assert(end010 === 0)
      assert(end01 === 0)
      assert(end000 === 1)
      assert(end00 === 1)
      assert(end0 === 1)
      assert(end === 1)
    }
    else if (end010 == 1) {
      assert(begin === 1)
      assert(begin0 === 1)
      assert(begin00 === 0)
      assert(begin000 === 0)
      assert(begin01 === 1)
      assert(begin010 === 1)
      assert(end010 === 1)
      assert(end01 === 1)
      assert(end000 === 0)
      assert(end00 === 0)
      assert(end0 === 1)
      assert(end === 1)
    }
/*    
    import scala.collection.mutable.ArrayBuffer

    describe("A empty array buffer") {
      val buf = ArrayBuffer[Int]() 
      
      describe("when 1 is inserted") {
        buf += 1 
        it("should have only 1 in it") {
          buf should be (ArrayBuffer(1)) 
          firstTestCount += 1
          inTest1FirstDescWas = firstDescCount
          inTest1SecondDescWas = secondDescCount
          inTest1OuterDescWas = outerDescCount
        }
        firstDescCount += 1
      }
      
      describe("when 2 is inserted") {
        buf += 2
        it("should have only 2 in it") {
          buf should be (ArrayBuffer(2))
          secondTestCount += 1
          inTest2FirstDescWas = firstDescCount
          inTest2SecondDescWas = secondDescCount
          inTest2OuterDescWas = outerDescCount
        }
        secondDescCount += 1
      }
      outerDescCount += 1
    }
*/
    override def newInstance = new MyFunSpec
  }
  
  object MyFunSpec {
       
    var instanceCount = 0
    var firstDescCount = 0
    var secondDescCount = 0
    var outerDescCount = 0
    var firstTestCount = 0
    var secondTestCount = 0
    var inTest1FirstDescWas = 0
    var inTest1SecondDescWas = 0
    var inTest1OuterDescWas = 0
    var inTest2FirstDescWas = 0
    var inTest2SecondDescWas = 0
    var inTest2OuterDescWas = 0
    
    def resetCounts() {
      instanceCount = 0
      firstDescCount = 0
      secondDescCount = 0
      outerDescCount = 0
      firstTestCount = 0
      secondTestCount = 0
      inTest1FirstDescWas = 0
      inTest1SecondDescWas = 0
      inTest1OuterDescWas = 0
      inTest2FirstDescWas = 0
      inTest2SecondDescWas = 0
      inTest2OuterDescWas = 0
    }
  }
      
  describe("A path.FunSpec") {
    
    it("should execute the first test, and only the first test, on initial instance creation") {
      {
        import MyFunSpec._
        resetCounts()
        val mySpec = new MyFunSpec() 
        assert(firstTestCount === 1)
        assert(secondTestCount === 0)
      }
    }

    ignore("should create an one instance per test, running each describe clause once plus once per path ") {
      import MyFunSpec._
      resetCounts()
      val mySpec = new MyFunSpec()
      mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(instanceCount === 2)
      assert(firstDescCount === 1)
      assert(secondDescCount === 1)
      assert(outerDescCount === 2)
    }
    
    ignore("should execute each test once") {
      import MyFunSpec._
      resetCounts()
      val mySpec = new MyFunSpec()
      mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(firstTestCount === 1)
      assert(secondTestCount === 1)
    }
    
    ignore("should execute each test before anything textually after the tests") {
      import MyFunSpec._
      resetCounts()
      val mySpec = new MyFunSpec()
      mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(inTest1FirstDescWas === 0)
      assert(inTest1SecondDescWas === 0)
      assert(inTest1OuterDescWas === 0)
      assert(inTest2FirstDescWas === 1)
      assert(inTest2SecondDescWas === 0)
      assert(inTest2OuterDescWas === 2)
    }
 
    class AllResultsSpec extends org.scalatest.path.FunSpec {
      it("should succeed") {
        assert(1 + 1 === 2)
      }
      it("should fail") {
        assert(1 + 1 === 3)
      }
      it("should be pending") (pending)
      ignore("should be ignored") {
        assert(1 + 1 === 3)
      }
      // TODO in 2.0, add a canceled test
      override def newInstance = new AllResultsSpec
	}

    ignore("should report a sucessful/failed/pending/ignored tests correctly") {

      val mySpec = new AllResultsSpec()
      val repo = new EventRecordingReporter
      mySpec.run(None, repo, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(repo.testSucceededEventsReceived.size === 1)
      assert(repo.testFailedEventsReceived.size === 1)
      assert(repo.testPendingEventsReceived.size === 1)
      assert(repo.testIgnoredEventsReceived.size === 1)
    }
  }
}

 /* Will disallow parallel in path traits probably.
  
  class MyParallelFunSpec extends org.scalatest.path.FunSpec with ShouldMatchers with ParallelTestExecution {
    
    import scala.collection.mutable.ListBuffer
    import MyParallelFunSpec._
    
    instanceCount += 1 
    
    describe("An empty list") {
      val list = ListBuffer[Int]() 
      
      describe("when 1 is inserted") {
        list += 1 
        it("should have only 1 in it") {
          list should be (ListBuffer(1)) 
          firstTestCount += 1
          inTest1FirstDescWas = firstDescCount
          inTest1SecondDescWas = secondDescCount
          inTest1OuterDescWas = outerDescCount
          val threadName = Thread.currentThread
          println("Test1: " + threadName)
        }
        firstDescCount += 1
      }
      
      describe("when 2 is inserted") {
        list += 2
        it("should have only 2 in it") {
          list should be (ListBuffer(2))
          secondTestCount += 1
          inTest2FirstDescWas = firstDescCount
          inTest2SecondDescWas = secondDescCount
          inTest2OuterDescWas = outerDescCount
          val threadName = Thread.currentThread
          println("Test2: " + threadName)
        }
        secondDescCount += 1
      }
      outerDescCount += 1
    }
    
    override def newInstance = new MyParallelFunSpec
  }
  
  object MyParallelFunSpec {
    import scala.collection.mutable.ListBuffer
    
    @volatile var instanceCount = 0
    @volatile var firstDescCount = 0
    @volatile var secondDescCount = 0
    @volatile var outerDescCount = 0
    @volatile var firstTestCount = 0
    @volatile var secondTestCount = 0
    @volatile var inTest1FirstDescWas = 0
    @volatile var inTest1SecondDescWas = 0
    @volatile var inTest1OuterDescWas = 0
    @volatile var inTest2FirstDescWas = 0
    @volatile var inTest2SecondDescWas = 0
    @volatile var inTest2OuterDescWas = 0
    
    def resetCounts() {
      instanceCount = 0
      firstDescCount = 0
      secondDescCount = 0
      outerDescCount = 0
      firstTestCount = 0
      secondTestCount = 0
      inTest1FirstDescWas = 0
      inTest1SecondDescWas = 0
      inTest1OuterDescWas = 0
      inTest2FirstDescWas = 0
      inTest2SecondDescWas = 0
      inTest2OuterDescWas = 0
    }
  }
  
  describe("A parallel path.FunSpec") {
    
    import MyParallelFunSpec._
    it("should create an one instance per test, running each describe clause once plus once per path, when no distributor is passed") {
      resetCounts()
      println("Num 1")
      val mySpec = new MyParallelFunSpec()
      mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(instanceCount === 3)
      assert(firstDescCount === 2)
      assert(secondDescCount === 2)
      assert(outerDescCount === 3)
    }
    
    it("should execute each test once, when no distributor is passed") {
      resetCounts()
      println("Num 2")
      val mySpec = new MyParallelFunSpec()
      mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(firstTestCount === 1)
      assert(secondTestCount === 1)
    }
    
    it("should execute each test before anything textually after the tests, when no distributor is passed") {
      resetCounts()
      println("Num 3")
      val mySpec = new MyParallelFunSpec()
      mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(inTest1FirstDescWas === 1)
      assert(inTest1SecondDescWas === 1)
      assert(inTest1OuterDescWas === 1)
      assert(inTest2FirstDescWas === 2)
      assert(inTest2SecondDescWas === 1)
      assert(inTest2OuterDescWas === 2)
    }
 
    def withConcurrentDistributor(fun: Distributor => Unit) {
      val poolSize = 2
      val execSvc: ExecutorService = Executors.newFixedThreadPool(poolSize)
      try {
        val dispatcher = new DispatchReporter(List(SilentReporter), System.out)
        val distributor = new ConcurrentDistributor(dispatcher, new Stopper {}, Filter(), Map(), execSvc)
        fun(distributor)
        distributor.waitUntilDone()
      }
      finally {
        execSvc.shutdown()
      }
    }
    
    it("should create an one instance per test, running each describe clause once plus once per path, when a distributor is passed") {
      resetCounts()
      println("Num 4")
      val mySpec = new MyParallelFunSpec()
      withConcurrentDistributor { distributor =>
        mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), Some(distributor), new Tracker())
      }
      assert(instanceCount === 3)
      assert(firstDescCount === 2)
      assert(secondDescCount === 2)
      assert(outerDescCount === 3)
    }
  
    it("should execute each test once, when a distributor is passed") {
      resetCounts()
      println("Num 5")
      val mySpec = new MyParallelFunSpec()
      withConcurrentDistributor { distributor =>
        mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), Some(distributor), new Tracker())
      }
      assert(firstTestCount === 1)
      assert(secondTestCount === 1)
    }
    
    it("should execute each test before anything textually after the tests, when a distributor is passed") {
      resetCounts()
      println("Num 6")
      val mySpec = new MyParallelFunSpec()
      withConcurrentDistributor { distributor =>
        mySpec.run(None, SilentReporter, new Stopper {}, Filter(), Map(), Some(distributor), new Tracker())
      }
      assert(inTest1FirstDescWas === 1)
      assert(inTest1SecondDescWas === 1)
      assert(inTest1OuterDescWas >= 1) // Because now in parallel
      assert(inTest2FirstDescWas >= 1) // Because now in parallel
      assert(inTest2SecondDescWas === 1)
      assert(inTest2OuterDescWas >= 1) // Because now in parallel
    }
 
    class AllResultsSpec extends org.scalatest.path.FunSpec with ParallelTestExecution {
      it("should succeed") {
        assert(1 + 1 === 2)
      }
      it("should fail") {
        assert(1 + 1 === 3)
      }
      it("should be pending") (pending)
      ignore("should be ignored") {
        assert(1 + 1 === 3)
      }
      // TODO in 2.0, add a canceled test
      override def newInstance = new AllResultsSpec
	}

    it("should report a sucessful/failed/pending/ignored tests correctly") {

      val mySpec = new AllResultsSpec()
      val repo = new EventRecordingReporter
      mySpec.run(None, repo, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(repo.testSucceededEventsReceived.size === 1)
      assert(repo.testFailedEventsReceived.size === 1)
      assert(repo.testPendingEventsReceived.size === 1)
      assert(repo.testIgnoredEventsReceived.size === 1)
    }
  }
  */

