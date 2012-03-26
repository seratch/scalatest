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

class ScalaTestReporter(reporter: Reporter, formatter: Formatter, dispatch: org.scalatest.Reporter) extends Reporter with Formatter {

  private val steps = new ListBuffer[Step]()
  
  def result(result: Result) {
    val error = result.getError
    if (Result.SKIPPED == result || Result.UNDEFINED == result) {
      // Ignore, may be UNDEFINED for cancel?
    }
    else if (error != null) {
      if (error.isInstanceOf[PendingException]) {
        // Pending
      }
      else {
        // Failed
      }
    }
    else {
      // Success
    }
    reporter.result(result)
  }
  
  def `match`(m: Match) {
    // What is this doing??
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