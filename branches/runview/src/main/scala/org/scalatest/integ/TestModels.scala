package org.scalatest.integ

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import TestStatus._
import ScopeStatus._
import SuiteStatus._
import RunStatus._
import javax.swing.tree.TreeNode

final case class Summary(testsSucceededCount: Int, testsFailedCount: Int, testsIgnoredCount: Int, testsPendingCount: Int, testsCanceledCount: Int,
  suitesCompletedCount: Int, suitesAbortedCount: Int) {
  val testsCompletedCount = testsSucceededCount + testsFailedCount
}

case class TestNameInfo(testName: String, decodedTestName: Option[String])

final case class NameInfo(suiteName: String, suiteId: String, suiteClassName: Option[String], decodedSuiteName:Option[String],  testName: Option[TestNameInfo])

case class StackTraceElement(className: String, methodName: String, fileName: String, lineNumber: Int, isNative: Boolean, toStringValue: String) {
  override def toString = toStringValue
}

sealed abstract class Node extends TreeNode {
  private var childrenBuffer = new ListBuffer[Node]()
  var parent: Node = null
  def addChild(child: Node) {
    synchronized {
      childrenBuffer += child
      child.parent = this
    }
  } 
  def children = collection.JavaConversions.asEnumeration(childrenBuffer.iterator)
  def childrenList = childrenBuffer.toList
  def hasChildren = childrenBuffer.size > 0
  def getStackTraces: Option[Array[StackTraceElement]]
  def getStackDepth: Option[Int]
  def getLocation: Option[Location]
  
  def getAllowsChildren = true
  def getChildAt(childIndex: Int) = childrenBuffer.toList(childIndex)
  def getChildCount = childrenBuffer.size
  def getIndex(node: TreeNode) = childrenBuffer.indexOf(node)
  def getParent = parent
  def isLeaf = childrenBuffer.size == 0
}

final case class TestModel(
  suiteId: String, 
  suiteClassName: Option[String],
  testName: String,
  testText: String,
  decodedTestName: Option[String],
  var duration: Option[Long],
  var errorMessage: Option[String], 
  var errorDepth: Option[Int], 
  var errorStackTrace: Option[Array[StackTraceElement]], 
  var location: Option[Location],
  rerunner: Option[String],
  threadName: String,
  timeStamp: Long, 
  var status: TestStatus
) extends Node {
  def getStackTraces = errorStackTrace
  def getStackDepth = errorDepth
  def getLocation = location
}

final case class ScopeModel(
  message: String,
  nameInfo: NameInfo,
  location: Option[Location],
  threadName: String,
  timeStamp: Long, 
  var status: ScopeStatus
) extends Node {
  
  def scopeSucceed: Boolean = {
    childrenList.forall { child => 
      child match {
        case test: TestModel => 
          test.status != TestStatus.FAILED
        case scope: ScopeModel => 
          scope.scopeSucceed
        case _ =>
          true
      }
    }
  }
  
  def getStackTraces = None
  def getStackDepth = None
  def getLocation = location
}

final case class SuiteModel(
  suiteName: String,
  suiteId: String,
  suiteClassName: Option[String],
  decodedSuiteName: Option[String],
  var location: Option[Location],
  rerunner: Option[String],
  var duration: Option[Long] = None,
  var errorMessage: Option[String], 
  var errorDepth: Option[Int], 
  var errorStackTrace: Option[Array[StackTraceElement]], 
  threadName: String,
  timeStamp: Long, 
  var status: SuiteStatus
) extends Node {
  
  private val scopeStack: Stack[Node] = Stack[Node]()
  private var flatTestsCache: ListBuffer[TestModel] = new ListBuffer[TestModel]()
  
  override def addChild(child: Node) {
    if (scopeStack.isEmpty)
      super.addChild(child)
    else 
      scopeStack.head.addChild(child)
    
    child match {
      case scope: ScopeModel => 
        scopeStack.push(scope)
      case test: TestModel =>
        scopeStack.push(test)
        flatTestsCache += test
      case _ => 
        // Do nothing for other type of child
    }
  }
  
  def closeScope() {
    scopeStack.pop()
  }
  
  def updateTest(testName: String, status: TestStatus, duration: Option[Long], location: Option[Location], errorMessage: Option[String], errorDepth: Option[Int], errorStackTrace: Option[Array[StackTraceElement]]) = {
    val node = flatTestsCache.toArray.find(node => node.isInstanceOf[TestModel] && node.asInstanceOf[TestModel].testName == testName)
    node match {
      case Some(node) => 
        val test = node.asInstanceOf[TestModel]
        test.status = status
        test.duration = duration
        test.location = location
        test.errorMessage = errorMessage
        test.errorDepth = errorDepth
        test.errorStackTrace = errorStackTrace
        test
      case None => 
        // Should not happen
        throw new IllegalStateException("Unable to find test name: " + testName + ", suiteId: " + suiteId)
    }
  }
  
  def suiteSucceeded = {
    flatTestsCache.toArray.forall(child => child.status != TestStatus.FAILED)
  }
  
  def getStackTraces = errorStackTrace
  def getStackDepth = errorDepth
  def getLocation = location
}

final case class RunModel(
  testCount: Int, 
  var duration: Option[Long] = None,
  var summary: Option[Summary] = None,
  var errorMessage: Option[String], 
  var errorDepth: Option[Int],
  var errorStackTrace: Option[Array[StackTraceElement]], 
  threadName: String,
  timeStamp: Long, 
  var status: RunStatus
) extends Node {
  def getStackTraces = errorStackTrace
  def getStackDepth = errorDepth
  def getLocation = None
}

final case class InfoModel(
  message: String,
  nameInfo: Option[NameInfo],
  aboutAPendingTest: Option[Boolean],
  aboutACanceledTest: Option[Boolean],
  errorMessage: Option[String], 
  errorDepth: Option[Int],
  errorStackTrace: Option[Array[StackTraceElement]], 
  location: Option[Location], 
  threadName: String,
  timeStamp: Long
) extends Node {
  def getStackTraces = errorStackTrace
  def getStackDepth = errorDepth
  def getLocation = location
}