package org.scalatest.cucumber

import gherkin.formatter.Formatter
import gherkin.formatter.Reporter
import scala.collection.mutable.ListBuffer
import gherkin.formatter.model.Step
import gherkin.formatter.model.Result
import gherkin.formatter.model.Match
import java.io.InputStream
import cucumber.runtime.PendingException
import gherkin.formatter.model.Feature
import gherkin.formatter.model.Background
import gherkin.formatter.model.Scenario
import gherkin.formatter.model.ScenarioOutline
import gherkin.formatter.model.Examples
import org.scalatest.Tracker
import org.scalatest.events.TestStarting
import org.scalatest.events.MotionToSuppress
import org.scalatest.events.TestSucceeded
import org.scalatest.Suite
import org.scalatest.events.TestPending
import org.scalatest.events.TestFailed

class ScalaTestReporter(reporter: Reporter, formatter: Formatter, suiteClass: Class[_], tracker: Tracker, dispatch: org.scalatest.Reporter) extends Reporter with Formatter {

  private val steps = new ListBuffer[Step]()
  private var currentStep: Step = null
  private var indentLevel: Int = 0
  
  private def getTestName(step: Step) = step.getKeyword + ": " + step.getName
  
  def result(result: Result) {
    val error = result.getError
    val testName = getTestName(currentStep)
    val formatter = Suite.getIndentedText(testName, indentLevel + 1, true)
    
    if (Result.SKIPPED == result) {
      // Skipped, should map to Ignore
      //println("#####SKIPPED - " + getTestName(currentStep))
      dispatch(TestPending(tracker.nextOrdinal(), suiteClass.getSimpleName, Some(suiteClass.getName), testName, Some(formatter)))
    }
    else if (Result.UNDEFINED == result) {
      // This happens when step is not defined, should map to canceled?
      //println("####UNDEFINED - " + getTestName(currentStep))
      dispatch(TestFailed(tracker.nextOrdinal(), "Step is not defined.", suiteClass.getSimpleName, Some(suiteClass.getName), testName, None, Some(result.getDuration), Some(formatter), None))
    }
    else if (error != null) {
      if (error.isInstanceOf[PendingException]) {
        // Pending
        //println("#####PENDING - " + getTestName(currentStep))
        dispatch(TestPending(tracker.nextOrdinal(), suiteClass.getSimpleName, Some(suiteClass.getName), testName, Some(formatter)))
      }
      else {
        // Failed
        //println("#####FAILED - " + getTestName(currentStep))
        dispatch(TestFailed(tracker.nextOrdinal(), error.getMessage, suiteClass.getSimpleName, Some(suiteClass.getName), testName, Some(error), Some(result.getDuration), Some(formatter), None))
      }
    }
    else {
      // Success
      //println("#####SUCCESS - " + getTestName(currentStep))
      dispatch(TestSucceeded(tracker.nextOrdinal(), suiteClass.getSimpleName, Some(suiteClass.getName), testName, Some(result.getDuration), Some(formatter), None))
    }
    reporter.result(result)
  }
  
  def `match`(m: Match) {
    currentStep = steps.remove(0)
    //println("#####TEST STARTING - " + getTestName(currentStep))
    dispatch(TestStarting(tracker.nextOrdinal(), suiteClass.getSimpleName, Some(suiteClass.getName), getTestName(currentStep), Some(MotionToSuppress), None))
    reporter.`match`(m)
  }
  
  def embedding(mimeType:String, data: InputStream) {
    reporter.embedding(mimeType, data)
  }
  
  def write(text: String) {
    reporter.write(text)
  }
  
  def uri(uri: String) {
    formatter.uri(uri)
  }
  
  def feature(feature: Feature) {
    formatter.feature(feature)
  }
  
  def background(background: Background) {
    formatter.background(background)
  }
  
  def scenario(scenario: Scenario) {
    formatter.scenario(scenario)
  }
  
  def scenarioOutline(scenarioOutline: ScenarioOutline) {
    formatter.scenarioOutline(scenarioOutline)
  }
  
  def examples(examples: Examples) {
    formatter.examples(examples)
  }
  
  def step(step: Step) {
    steps += step
    formatter.step(step)
  }
  
  def eof() {
    formatter.eof()
  }
  
  def syntaxError(state: String, event: String, legalEvents: java.util.List[String], uri: String, line: Int) {
    formatter.syntaxError(state, event, legalEvents, uri, line)
  }
  
  def done() {
    formatter.done()
  }

  def close() {
    formatter.close()
  }
}