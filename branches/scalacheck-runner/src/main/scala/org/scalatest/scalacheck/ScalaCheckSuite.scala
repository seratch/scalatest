package org.scalatest.scalacheck

import org.scalatest.Suite
import org.scalacheck.Properties
import org.scalatest.Reporter
import org.scalatest.Stopper
import org.scalatest.Filter
import org.scalatest.Tracker
import org.scalatest.Distributor
import org.scalacheck.Test
import org.scalacheck.Gen
import org.scalacheck.Test.Result
import org.scalatest.events.TestStarting
import org.scalatest.events.MotionToSuppress
import scala.reflect.NameTransformer
import org.scalatest.events.TestSucceeded
import org.scalatest.events.TestFailed
import org.scalatest.events.TestPending
import org.scalatest.events.TestCanceled
import org.scalatest.Style
import org.scalatest.Args

@Style("org.scalatest.scalacheck.ScalaCheckFinder")
trait ScalaCheckSuite extends Suite { propSuite: Properties =>
  
  val prefix = propSuite.name + "."
  var currentTestName: String = null

  override def run(testName: Option[String], args: Args) {
    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")
    
    import args._

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(reporter)
    
    val testCallback = new Test.TestCallback {
      override def onPropEval(name: String, threadIdx: Int, succeeded: Int, discarded: Int) {
        //println("test starting: " + name)
      }

      override def onTestResult(name: String, result: Result) {
        val duration = result.time
        val formatter = Suite.getIndentedTextForTest(currentTestName, 0, true)
        result.status match {
          case Test.Passed => 
            report(TestSucceeded(tracker.nextOrdinal, propSuite.getClass.getSimpleName, suiteId, Some(propSuite.getClass.getName), getDecodedName(propSuite.getClass.getSimpleName), currentTestName, currentTestName, getDecodedName(currentTestName), IndexedSeq.empty, Some(duration), Some(formatter), None))
          case Test.Proved(args) =>
            report(TestSucceeded(tracker.nextOrdinal, propSuite.getClass.getSimpleName, suiteId, Some(propSuite.getClass.getName), getDecodedName(propSuite.getClass.getSimpleName), currentTestName, currentTestName, getDecodedName(currentTestName), IndexedSeq.empty, Some(duration), Some(formatter), None))
          case Test.Failed(args, labels) =>
            report(TestFailed(tracker.nextOrdinal, "Property test failed with: " + args.mkString(", "), propSuite.getClass.getSimpleName, propSuite.getClass.getName, Some(propSuite.getClass.getName), getDecodedName(propSuite.getClass.getSimpleName), currentTestName, currentTestName, getDecodedName(currentTestName), IndexedSeq.empty, None, Some(duration), Some(formatter), None))
          case Test.Exhausted =>
            report(TestCanceled(tracker.nextOrdinal, "Property test exhausted", propSuite.getClass.getSimpleName, suiteId, Some(propSuite.getClass.getName), getDecodedName(propSuite.getClass.getSimpleName), currentTestName, currentTestName, getDecodedName(currentTestName), IndexedSeq.empty, None, Some(duration), Some(formatter), None))
          case Test.PropException(args, throwable, labels) => 
            report(TestCanceled(tracker.nextOrdinal, "Encounter error when evaluating property: " + args.mkString(", "), propSuite.getClass.getSimpleName, suiteId, Some(propSuite.getClass.getName), getDecodedName(propSuite.getClass.getSimpleName), currentTestName, currentTestName, getDecodedName(currentTestName), IndexedSeq.empty, Some(throwable), Some(duration), Some(formatter), None))
          case Test.GenException(throwable) =>
            report(TestCanceled(tracker.nextOrdinal, "Encounter error when generating concrete arguments: " + throwable.getMessage, propSuite.getClass.getSimpleName, suiteId, Some(propSuite.getClass.getName), getDecodedName(propSuite.getClass.getSimpleName), currentTestName, currentTestName, getDecodedName(currentTestName), IndexedSeq.empty, Some(throwable), Some(duration), Some(formatter), None))
        }
      }
    }
    
    val tParams = Test.Params(100, 5, 0, Gen.Params().size, Gen.Params().rng, 1, testCallback)

    propSuite.properties.foreach { case (name, p) =>
      currentTestName = name.substring(prefix.length)
      val (filterTest, ignoreTest) = filter(currentTestName, tags, suiteId)
      if (!filterTest) {
        report(TestStarting(tracker.nextOrdinal(), propSuite.getClass.getSimpleName, suiteId, Some(propSuite.getClass.getName), getDecodedName(propSuite.getClass.getSimpleName), currentTestName, currentTestName, getDecodedName(currentTestName), Some(MotionToSuppress), None, Some(propSuite.getClass.getName)))
        Test.check(tParams, p)
      }
    }
  }
  
  override def expectedTestCount(filter: Filter): Int = {
    propSuite.properties.filter { case (name, p) =>
      val testName = name.substring(prefix.length)
      val (filterTest, ignoreTest) = filter(testName, tags, suiteId)
      !filterTest
    }.length
  }
  
  private def getDecodedName(name:String): Option[String] = {
    val decoded = NameTransformer.decode(name)
    if(decoded == name) None else Some(decoded)
  }
}