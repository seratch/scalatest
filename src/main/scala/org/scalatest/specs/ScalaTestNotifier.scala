package org.scalatest.specs
import org.specs.Specification
import org.specs.runner.Notifier
import scala.reflect.NameTransformer
import scala.collection.mutable.Stack
import org.scalatest.JReporter
import org.scalatest.JTracker
import org.scalatest.JFormatter

class ScalaTestNotifier(theSpec: Specification, suiteId: String, suiteName: String, theTracker: JTracker, reporter: JReporter) extends ScalaTestAbstractNotifier {
  val spec = theSpec
  val tracker = theTracker
  val report = reporter
  
  var systemStart: Long = 0
  var exampleStart: Long = 0
  var indentLevel: Int = 0
  
  private val scopeStack: Stack[String] = Stack[String]()
  
  def runStarting(examplesCount: Int) { }
  
  def getIndentedTextForInfo(message: String, level: Int, includeIcon: Boolean, infoIsInsideATest: Boolean) = {
    val formattedText =
      if (includeIcon) {
        val infoProvidedIcon = "+"
        //
        // Inside a test, you want level 1 to stay 1
        // [scalatest] - outermost test (5 milliseconds)
        // [scalatest]   + in outermost test
        //
        // But outside a test, level 1 should be transformed to 0
        // [scalatest] Apple
        // [scalatest] + in Apple
        //
        val indentationLevel =
          level match {
            case 0 => 0
            case 1 if infoIsInsideATest => 1
            case _ => level - 1
          }
        ("  " * indentationLevel) + infoProvidedIcon + " " + message
        // ("  " * (if (level <= 1) level else (level - 1))) + Resources("iconPlusShortName", infoProvidedIcon, message)
      }
      else {
        ("  " * level) + message
      }
    report.createIndentedText(formattedText, message, level)
  }
  
  def getIndentedTextForTest(testText: String, level: Int, includeIcon: Boolean) = {
    val decodedTestText = NameTransformer.decode(testText)
    val formattedText =
      if (includeIcon) {
        val testSucceededIcon = "-"
        ("  " * (if (level == 0) 0 else (level - 1))) + testSucceededIcon + " " + decodedTestText
      }
      else {
        ("  " * level) + decodedTestText
      }
    report.createIndentedText(formattedText, decodedTestText, level)
  }
  
  def scopeOpened(name: String) { 
    indentLevel += 1
    if (scopeStack.isEmpty)
      scopeStack.push("") // Ignore the first scope, which is same as the suiteName
    else // the scopeStack.length check is to make sure for the first scope "", there's no need for the space to concat.
      scopeStack.push(scopeStack.head + (if (scopeStack.length > 1) " " else "") + name)
    if (scopeStack.length > 1) {
      val formatter = getIndentedTextForInfo(name, indentLevel, false, false)
      report.fireScopeOpened(tracker.nextOrdinal, name, suiteName, suiteId, spec.getClass.getName, formatter, null, null)
    }
  }
  
  def scopeClosed(name: String) { 
    scopeStack.pop()
    if (scopeStack.length > 0) { // No need to fire for the last scope, which is the one same as the suiteName 
      val formatter = getIndentedTextForInfo(name, indentLevel, false, false)
      report.fireScopeClosed(tracker.nextOrdinal, name, suiteName, suiteId, spec.getClass.getName, formatter, null, null)
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
    val formatter = report.createMotionToSuppress
    report.fireTestStarting(tracker.nextOrdinal, suiteName, suiteId, spec.getClass.getName, testName, exampleName, formatter, null, spec.getClass.getName, null)
  }
  
  def exampleSucceeded(exampleName: String) {
    val duration = System.currentTimeMillis() - exampleStart
    val formatter = getIndentedTextForTest(exampleName, indentLevel + 1, true)
    val testName = getTestName(exampleName)
    report.fireTestSucceeded(tracker.nextOrdinal, suiteName, suiteId, spec.getClass.getName, testName, exampleName, Array.empty, 
                             duration, formatter, null, spec.getClass.getName, null)
    //report(TestSucceeded(tracker.nextOrdinal(), spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(testName), IndexedSeq.empty, Some(duration), Some(formatter), None))
  }
  
  def exampleFailed(exampleName: String, e: Throwable) {
    val duration = System.currentTimeMillis() - exampleStart
    val formatter = getIndentedTextForTest(exampleName, indentLevel + 1, true)
    val testName = getTestName(exampleName)
    report.fireTestFailed(tracker.nextOrdinal, e.getMessage, suiteName, suiteId, spec.getClass.getName, testName, exampleName, 
                          Array.empty, e, duration, formatter, null, spec.getClass.getName, null)
    //report(TestFailed(tracker.nextOrdinal(), e.getMessage, spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(testName), IndexedSeq.empty, Some(e), Some(duration), Some(formatter), None))
  }
  
  def exampleError(exampleName: String, e: Throwable) {
    val duration = System.currentTimeMillis() - exampleStart
    val formatter = getIndentedTextForTest(exampleName, indentLevel + 1, true)
    val testName = getTestName(exampleName)
    report.fireTestFailed(tracker.nextOrdinal, e.getMessage, suiteName, suiteId, spec.getClass.getName, testName, exampleName, 
                          Array.empty, e, duration, formatter, null, spec.getClass.getName, null)
    //report(TestFailed(tracker.nextOrdinal(), e.getMessage, spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(spec.getClass.getSimpleName), IndexedSeq.empty, Some(e), Some(duration), Some(formatter), None))
  }
  
  def exampleSkipped(exampleName: String) {
    val duration = System.currentTimeMillis() - exampleStart
    val formatter = getIndentedTextForTest(exampleName, indentLevel + 1, true)
    val testName = getTestName(exampleName)
    report.fireTestPending(tracker.nextOrdinal, suiteName, suiteId, spec.getClass.getName, testName, exampleName, Array.empty, 
                           duration, formatter, null, null)
    //report(TestPending(tracker.nextOrdinal(), spec.getClass.getSimpleName, spec.getClass.getName, Some(spec.getClass.getName), getDecodedName(spec.getClass.getSimpleName), testName, exampleName, getDecodedName(testName), IndexedSeq.empty, None, Some(formatter)))
  }
  
  def exampleCompleted(exampleName: String) { }
  
}