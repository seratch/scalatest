package org.scalatest.specs
import org.specs.Specification
import org.specs.runner.Notifier
import org.scalatest.Reporter
import org.scalatest.Tracker
import org.scalatest.events.IndentedText
import org.scalatest.events.SuiteStarting
import org.scalatest.events.InfoProvided
import org.scalatest.events.NameInfo
import org.scalatest.events.MotionToSuppress
import org.scalatest.events.SuiteCompleted
import org.scalatest.events.SuiteAborted
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.TestFailed
import org.scalatest.events.TestPending
import org.scalatest.Suite
import org.scalatest.events.ScopeOpened
import org.scalatest.events.ScopeClosed
import scala.reflect.NameTransformer
import scala.collection.mutable.Stack

class ScalaTestNotifier(theSpec: Specification, theTracker: Tracker, reporter: Reporter) extends ScalaTestAbstractNotifier {
  val spec = theSpec
  val tracker = theTracker
  val report = reporter
  
  var systemStart: Long = 0
  var exampleStart: Long = 0
  var indentLevel: Int = 0
  
  private val scopeStack: Stack[String] = Stack[String]()
  
  def runStarting(examplesCount: Int) { }
  
  def scopeOpened(name: String) { 
    indentLevel += 1
    if (scopeStack.isEmpty)
      scopeStack.push("") // Ignore the first scope, which is same as the suiteName
    else // the scopeStack.length check is to make sure for the first scope "", there's no need for the space to concat.
      scopeStack.push(scopeStack.head + (if (scopeStack.length > 1) " " else "") + name)
    if (scopeStack.length > 1) {
      val formatter = Suite.getIndentedTextForInfo(name, indentLevel, false, false)
      report(ScopeOpened(tracker.nextOrdinal, name, NameInfo(name, Some(spec.getClass.getName), Some(name)),
                         None, None, Some(formatter)))
    }
  }
  
  def scopeClosed(name: String) { 
    scopeStack.pop()
    if (scopeStack.length > 0) { // No need to fire for the last scope, which is the one same as the suiteName 
      val formatter = Suite.getIndentedTextForInfo(name, indentLevel, false, false)
      report(ScopeClosed(tracker.nextOrdinal, name, NameInfo(name, Some(spec.getClass.getName), Some(name)),
                         None, None, Some(MotionToSuppress)))
    }
    indentLevel -= 1
  }
  
  def getTestName(testText: String) = {
    if (scopeStack.isEmpty)
      testText
    else // the scopeStack.length check is to make sure for the first scope "", there's no need for the space to concat.
      scopeStack.head + (if (scopeStack.length > 1) " " else "") + testText 
  }
  
  def systemStarting(systemName: String) {
    systemStart = System.currentTimeMillis
    scopeOpened(systemName)
    //val formatter = Suite.getIndentedTextForInfo(systemName, indentLevel, false, false)
    //report(InfoProvided(tracker.nextOrdinal(), systemName, Some(NameInfo(systemName, Some(spec.getClass.getName), Some(systemName))), None, None, None, Some(formatter)))
  }
  
  def systemCompleted(systemName: String) { 
    scopeClosed(systemName)
  }
  
  def systemFailed(name: String, e: Throwable) { }
  
  def systemError(name: String, e: Throwable) { }
  
  def systemSkipped(name: String) { }
  
  def systemSucceeded(name: String) { }
  
  private def getDecodedName(name:String): Option[String] = {
    val decoded = NameTransformer.decode(name)
    if(decoded == name) None else Some(decoded)
  }
  
  def exampleStarting(exampleName: String) {
    exampleStart = System.currentTimeMillis()
    val testName = getTestName(exampleName)
    report(TestStarting(tracker.nextOrdinal(), spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(testName), Some(MotionToSuppress), None, Some(spec.getClass.getName)))
  }
  
  def exampleSucceeded(exampleName: String) {
    val duration = System.currentTimeMillis() - exampleStart
    val formatter = Suite.getIndentedTextForTest(exampleName, indentLevel + 1, true)
    val testName = getTestName(exampleName)
    report(TestSucceeded(tracker.nextOrdinal(), spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(testName), IndexedSeq.empty, Some(duration), Some(formatter), None))
  }
  
  def exampleFailed(exampleName: String, e: Throwable) {
    val duration = System.currentTimeMillis() - exampleStart
    val formatter = Suite.getIndentedTextForTest(exampleName, indentLevel + 1, true)
    val testName = getTestName(exampleName)
    report(TestFailed(tracker.nextOrdinal(), e.getMessage, spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(testName), IndexedSeq.empty, Some(e), Some(duration), Some(formatter), None))
  }
  
  def exampleError(exampleName: String, e: Throwable) {
    val duration = System.currentTimeMillis() - exampleStart
    val formatter = Suite.getIndentedTextForTest(exampleName, indentLevel + 1, true)
    val testName = getTestName(exampleName)
    report(TestFailed(tracker.nextOrdinal(), e.getMessage, spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(spec.getClass.getSimpleName), IndexedSeq.empty, Some(e), Some(duration), Some(formatter), None))
  }
  
  def exampleSkipped(exampleName: String) {
    val formatter = Suite.getIndentedTextForTest(exampleName, indentLevel + 1, true)
    val testName = getTestName(exampleName)
    report(TestPending(tracker.nextOrdinal(), spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(testName), IndexedSeq.empty, None, Some(formatter)))
  }
  
  def exampleCompleted(exampleName: String) { }
  
}