package org.scalatest.integ.ui

import java.util.Observable
import org.scalatest.integ.SuiteModel
import org.scalatest.integ.RunModel
import org.scalatest.integ.TestStarting
import org.scalatest.integ.TestModel
import org.scalatest.integ.Event
import org.scalatest.integ.TestStatus
import org.scalatest.integ.TestSucceeded
import org.scalatest.integ.TestFailed
import org.scalatest.integ.TestIgnored
import org.scalatest.integ.TestPending
import org.scalatest.integ.TestCanceled
import org.scalatest.integ.SuiteStarting
import org.scalatest.integ.SuiteStatus
import org.scalatest.integ.SuiteCompleted
import org.scalatest.integ.SuiteAborted
import org.scalatest.integ.RunStarting
import org.scalatest.integ.RunStatus
import org.scalatest.integ.RunCompleted
import org.scalatest.integ.RunStopped
import org.scalatest.integ.RunAborted
import org.scalatest.integ.InfoProvided
import org.scalatest.integ.InfoModel
import org.scalatest.integ.MarkupProvided
import org.scalatest.integ.ScopeOpened
import org.scalatest.integ.ScopeModel
import org.scalatest.integ.ScopeStatus
import org.scalatest.integ.ScopeClosed
import scala.annotation.tailrec
import org.scalatest.integ.Node

class ResultController extends Observable {
  
  @volatile var startedCount = 0
  @volatile var succeedCount = 0
  @volatile var failureCount = 0
  @volatile var ignoredCount = 0
  @volatile var pendingCount = 0
  @volatile var canceledCount = 0
  @volatile var totalCount = 0
  @volatile var suiteCount = 0
  @volatile var suiteAbortedCount = 0
  
  private var suiteMap: Map[String, SuiteModel] = null
  private var run: RunModel = null
  
  private var flattenNodeList: List[Node] = null
  
  def notifyChanges(value: AnyRef) {
    setChanged()
    notifyObservers(value)
  }
  
  def update(event: Event) {
    event match {
      case testStarting: TestStarting => 
        startedCount += 1
        val test = 
          TestModel(
            testStarting.suiteId, 
            testStarting.testName,
            testStarting.testText,
            testStarting.decodedTestName,
            None,
            None, 
            None, 
            None, 
            testStarting.location,
            testStarting.rerunner,
            testStarting.threadName,
            testStarting.timeStamp, 
            TestStatus.STARTED
          )
        suiteMap.get(testStarting.suiteId) match {
          case Some(suite) => 
            suite.addChild(test)
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestStarting, suiteId: " + testStarting.suiteId + ", test name: " + testStarting.testName)
        }
      case testSucceeded: TestSucceeded => 
        succeedCount += 1
        suiteMap.get(testSucceeded.suiteId) match {
          case Some(suite) => 
            val test = suite.updateTest(testSucceeded.testName, TestStatus.SUCCEEDED, testSucceeded.duration, testSucceeded.location, None, None, None)
            suite.closeScope()
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestSucceeded, suiteId: " + testSucceeded.suiteId + ", test name: " + testSucceeded.testName)
        }
      case testFailed: TestFailed => 
        failureCount += 1
        suiteMap.get(testFailed.suiteId) match {
          case Some(suite) => 
            val test = suite.updateTest(testFailed.testName, TestStatus.FAILED, testFailed.duration, testFailed.location, testFailed.errorMessage, testFailed.errorDepth, testFailed.errorStackTraces)
            suite.closeScope()
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestFailed, suiteId: " + testFailed.suiteId + ", test name: " + testFailed.testName)
        }
      case testIgnored: TestIgnored => 
        ignoredCount += 1
        val test = 
          TestModel(
            testIgnored.suiteId, 
            testIgnored.testName,
            testIgnored.testText,
            testIgnored.decodedTestName,
            None,
            None, 
            None, 
            None, 
            testIgnored.location,
            None,
            testIgnored.threadName,
            testIgnored.timeStamp, 
            TestStatus.IGNORED
          )
        suiteMap.get(testIgnored.suiteId) match {
          case Some(suite) => 
            suite.addChild(test)
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestIgnored, suiteId: " + testIgnored.suiteId + ", test name: " + testIgnored.testName)
        }
      case testPending: TestPending => 
        pendingCount += 1
        suiteMap.get(testPending.suiteId) match {
          case Some(suite) => 
            val test = suite.updateTest(testPending.testName, TestStatus.PENDING, testPending.duration, testPending.location, None, None, None)
            suite.closeScope()
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestPending, suiteId: " + testPending.suiteId + ", test name: " + testPending.testName)
        }
      case testCanceled: TestCanceled => 
        canceledCount += 1
        suiteMap.get(testCanceled.suiteId) match {
          case Some(suite) => 
            val test = suite.updateTest(testCanceled.testName, TestStatus.CANCELED, testCanceled.duration, testCanceled.location, testCanceled.errorMessage, testCanceled.errorDepth, testCanceled.errorStackTraces)
            suite.closeScope()
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestCanceled, suiteId: " + testCanceled.suiteId + ", test name: " + testCanceled.testName)
        }
      case suiteStarting: SuiteStarting => 
        if (suiteStarting.suiteId != "org.scalatest.tools.DiscoverySuite") {
          suiteCount += 1
          val suite = SuiteModel(
                        suiteStarting.suiteName,
                        suiteStarting.suiteId,
                        suiteStarting.suiteClassName,
                        suiteStarting.decodedSuiteName,
                        suiteStarting.location,
                        suiteStarting.rerunner,
                        None,
                        None,
                        None, 
                        None, 
                        suiteStarting.threadName,
                        suiteStarting.timeStamp, 
                        SuiteStatus.STARTED
                      )
          run.addChild(suite)
          suiteMap += (suite.suiteId -> suite)
          //fTestRunSession.rootNode.addChild(suite)
          notifyChanges(suite)
        }
      case suiteCompleted: SuiteCompleted => 
        if (suiteCompleted.suiteId != "org.scalatest.tools.DiscoverySuite") {
          suiteMap.get(suiteCompleted.suiteId) match {
            case Some(suite) => 
              suite.duration = suiteCompleted.duration
              suite.location = suiteCompleted.location
              suite.status = 
                if (suite.suiteSucceeded)
                  SuiteStatus.SUCCEED
                else
                  SuiteStatus.FAILED
              notifyChanges(suite)
            case None => 
              // Should not happen
              throw new IllegalStateException("Unable to find suite model for SuiteCompleted, suiteId: " + suiteCompleted.suiteId)
          }
        }
      case suiteAborted: SuiteAborted => 
        if (suiteAborted.suiteId != "org.scalatest.tools.DiscoverySuite") {
          suiteAbortedCount += 1
          suiteMap.get(suiteAborted.suiteId) match {
            case Some(suite) => 
              suite.duration = suiteAborted.duration
              suite.location = suiteAborted.location
              suite.errorMessage = suiteAborted.errorMessage
              suite.errorDepth = suiteAborted.errorDepth
              suite.errorStackTrace = suiteAborted.errorStackTraces
              suite.status = SuiteStatus.ABORTED
              notifyChanges(suite)
            case None => 
              // Should not happend
              throw new IllegalStateException("Unable to find suite model for SuiteAborted, suiteId: " + suiteAborted.suiteId)
          }
        }
      case runStarting: RunStarting => 
        //enableToolbarControls(false)
        startedCount = 0
        succeedCount = 0
        failureCount = 0
        ignoredCount = 0
        pendingCount = 0
        canceledCount = 0
        totalCount = runStarting.testCount
        suiteCount = 0
        suiteAbortedCount = 0
        suiteMap = Map.empty[String, SuiteModel]
        totalCount = runStarting.testCount
        run = 
          RunModel(
            runStarting.testCount, 
            None, 
            None,
            None,
            None, 
            None, 
            runStarting.threadName,
            runStarting.timeStamp, 
            RunStatus.STARTED
          )
        notifyChanges(run)
      case runCompleted: RunCompleted =>
        run.duration = runCompleted.duration
        run.summary = runCompleted.summary
        run.status = RunStatus.COMPLETED
        flattenNodeList = getFlattenNode(run.childrenList)
        notifyChanges(run)
        //enableToolbarControls(true)
      case runStopped: RunStopped => 
        run.duration = runStopped.duration
        run.summary = runStopped.summary
        run.status = RunStatus.STOPPED
        flattenNodeList = getFlattenNode(run.childrenList)
        notifyChanges(run)
        //enableToolbarControls(true)
      case runAborted: RunAborted => 
        if (run != null) {
          run.duration = runAborted.duration
          run.summary = runAborted.summary
          run.errorMessage = runAborted.errorMessage
          run.errorDepth = runAborted.errorDepth
          run.errorStackTrace = runAborted.errorStackTraces
          run.status = RunStatus.ABORTED
          notifyChanges(run)
        }
        flattenNodeList = getFlattenNode(run.childrenList)
        //enableToolbarControls(true)
      case infoProvided: InfoProvided => 
        val info = 
          InfoModel(
            infoProvided.message,
            infoProvided.nameInfo,
            infoProvided.aboutAPendingTest,
            infoProvided.aboutACanceledTest,
            infoProvided.errorMessage, 
            infoProvided.errorDepth, 
            infoProvided.errorStackTraces, 
            infoProvided.location, 
            infoProvided.threadName,
            infoProvided.timeStamp
          )
        infoProvided.nameInfo match {
          case Some(nameInfo) => 
            suiteMap.get(nameInfo.suiteId) match {
              case Some(suite) => 
                suite.addChild(info)
                notifyChanges(info)
              case None => 
                // Should not happen
               throw new IllegalStateException("Unable to find suite model for InfoProvided, suiteId: " + nameInfo.suiteId)
            }
          case None => 
            run.addChild(info)
        }
      case markupProvided: MarkupProvided => 
        // Do nothing for MarkupProvided, markup info should be shown in HtmlReporter only.
      case scopeOpened: ScopeOpened => 
        suiteMap.get(scopeOpened.nameInfo.suiteId) match {
          case Some(suite) => 
            val scope = 
              ScopeModel(
                scopeOpened.message,
                scopeOpened.nameInfo,
                scopeOpened.location,
                scopeOpened.threadName,
                scopeOpened.timeStamp, 
                ScopeStatus.OPENED
              )
            suite.addChild(scope)
            notifyChanges(scope)
          case None => 
            // Should not happend
            throw new IllegalStateException("Unable to find suite model for ScopeOpened, suiteId: " + scopeOpened.nameInfo.suiteId)
        }
      case scopeClosed: ScopeClosed => 
        suiteMap.get(scopeClosed.nameInfo.suiteId) match {
          case Some(suite) => 
            suite.closeScope()
          case None => 
            throw new IllegalStateException("Unable to find suite model for ScopeClosed, suiteId: " + scopeClosed.nameInfo.suiteId)
        }
      case _ => // Ignore others
    }
  }
  
  private def getFlattenNode(nodeList: List[Node]) = {
    @tailrec
    def getFlattenNodeAcc(acc: List[Node], nodeList: List[Node]): List[Node] = {
      nodeList match {
        case Nil => 
          acc
        case head :: rest =>
          getFlattenNodeAcc(head :: acc, head.childrenList.toList ::: rest)
      }
    }
    getFlattenNodeAcc(List.empty, nodeList).reverse
  }
  
  def findNextFailure(selectedNode: Node) = {
    val currentList = 
      if (selectedNode != null) {
        val dropList = flattenNodeList.dropWhile(node => node != selectedNode)
        if (dropList.isEmpty)
          Nil
        else
          dropList.tail
      }
      else
        flattenNodeList
    currentList.find { node => 
      node match {
        case test: TestModel if test.status == TestStatus.FAILED => true
        case _ => false
      }
    }.getOrElse(null)
  }
  
  def findPreviousFailure(selectedNode: Node) = {
    val currentList = 
      if (selectedNode != null) 
        flattenNodeList.takeWhile(node => node != selectedNode).reverse
      else
        flattenNodeList.reverse
    currentList.find { node => 
      node match {
        case test: TestModel if test.status == TestStatus.FAILED => true
        case _ => false
      }
    }.getOrElse(null)
  }
}