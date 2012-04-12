package org.scalatest.specs
import org.specs.Specification
import org.scalatest.Suite
import org.scalatest.Filter
import org.scalatest.Stopper
import org.scalatest.Reporter
import org.scalatest.Distributor
import org.scalatest.Tracker
import org.scalatest.Resources
import org.scalatest.events.InfoProvided
import org.scalatest.events.NameInfo
import org.specs.runner.NotifierRunner
import org.specs.specification.Example
import org.specs.specification.Sus
import org.scalatest.Style
import scala.collection.mutable.Stack

@Style("org.scalatest.specs.Spec1Finder")
class Spec1Runner(specificationClass: Class[_ <: Specification]) extends Suite { thisSuite =>
  
  val spec = specificationClass.newInstance()
  
  override def suiteName = specificationClass.getSimpleName
  override def suiteId = specificationClass.getName
  
  private def getSpecTestCount(theSpec: Specification, filter: Filter): Int = {
    theSpec.systems.foldLeft(0)(_ + getSusTestCount(_, filter, Stack[String]())) +
    theSpec.subSpecifications.foldLeft(0)(_ + getSpecTestCount(_, filter))
  }
  
  private def getSusTestCount(sus: Sus, filter: Filter, scopeStack: Stack[String]): Int = {
    val sysDesc = sus.description + " " + sus.verb
    if (scopeStack.isEmpty)
      scopeStack.push(sysDesc) 
    else 
      scopeStack.push(scopeStack.head + " " + sysDesc)
      
    val count = sus.examples.foldLeft(0)(_ + getExampleTestCount(_, filter, scopeStack))
    
    scopeStack.pop()
    
    count
  }
  
  private def getExampleTestCount(example: Example, filter: Filter, scopeStack: Stack[String]): Int = {
    if (example.hasSubExamples) {
      if (scopeStack.isEmpty)
      scopeStack.push(example.description) 
    else 
      scopeStack.push(scopeStack.head + " " + example.description)
      
      val count = example.examples.map(getExampleTestCount(_, filter, scopeStack)).foldLeft(0)(_ + _) + 1
      
      scopeStack.pop()
      count
    }
    else {
      val testName = getTestName(scopeStack, example.description)
      val (filterExample, ignoreTest) = filter(testName, tags, suiteId)
      if (!filterExample)
        1
      else
        0
    }
    /*if(example.hasSubExamples)
      example.examples.map(getExampleTestCount(_)).foldLeft(0)(_ + _)
    else
      example.ownExpectationsNb*/
  }
  
  private def getTestName(scopeStack: Stack[String], testText: String) = {
    if (scopeStack.isEmpty)
      testText
    else // the scopeStack.length check is to make sure for the first scope "", there's no need for the space to concat.
      scopeStack.head + " " + testText 
  }
  
  override def expectedTestCount(filter: Filter): Int = getSpecTestCount(spec, filter)//spec.firstLevelExamplesNb
  
  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
              configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
    if (testName == null)
      throw new NullPointerException("testName was null")
    if (reporter == null)
      throw new NullPointerException("reporter was null")
    if (stopper == null)
      throw new NullPointerException("stopper was null")
    if (filter == null)
      throw new NullPointerException("filter was null")
    if (configMap == null)
      throw new NullPointerException("configMap was null")
    if (distributor == null)
      throw new NullPointerException("distributor was null")
    if (tracker == null)
      throw new NullPointerException("tracker was null")

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(reporter)
    
    runSpec(Some(spec), tracker, reporter, filter)
    
    if (stopRequested()) {
      val rawString = Resources("executeStopping")
      report(InfoProvided(tracker.nextOrdinal(), rawString, Some(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), testName))))
    }
  }
  
  def runSpec(specification: Option[Specification], tracker: Tracker, reporter: Reporter, filter: Filter): Option[Specification] = {
    def testInterfaceRunner(s: Specification) = new ScalaTestNotifierRunner(s, new ScalaTestNotifier(spec, tracker, reporter), filter, tags, suiteId)
    specification.map(testInterfaceRunner(_).reportSpecs)
    specification match {
      case Some(s: org.specs.runner.File) => s.reportSpecs
      case _ => ()
    }
    specification
  }
}