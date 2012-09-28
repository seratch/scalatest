package org.scalatest.specs

import org.scalatest.JSuite
import org.specs.Specification
import org.scalatest.JFilter
import org.specs.specification.Sus
import scala.collection.mutable.Stack
import org.specs.specification.Example
import scala.collection.JavaConversions._
import org.scalatest.JArgs
import org.scalatest.JReporter
import org.scalatest.JTracker

class Specs1Suite(specificationClass: Class[_ <: Specification]) extends JSuite {
  
  val spec = specificationClass.newInstance
  
  def runSpec(specification: Option[Specification], tracker: JTracker, reporter: JReporter, filter: JFilter): Option[Specification] = {
    def testInterfaceRunner(s: Specification) = new ScalaTestNotifierRunner(s, new ScalaTestNotifier(spec, suiteId, suiteName, tracker, reporter), filter, tags, suiteId)
    specification.map(testInterfaceRunner(_).reportSpecs)
    specification match {
      case Some(s: org.specs.runner.File) => s.reportSpecs
      case _ => ()
    }
    specification
  }
  
  def run(testName: String, args: JArgs) {
    if (args == null)
      throw new NullPointerException("args was null")
    
    import args._
    
    runSpec(Some(spec), tracker, reporter, filter)
    
    if (stopper.isStopRequested) {
      val rawString = "The run method of a Suite is returning because a stop was requested."
      reporter.fireInfoProvided(tracker.nextOrdinal, rawString, suiteName, suiteId, spec.getClass.getName, testName, 
                                null, null, null, null)
      //report(InfoProvided(tracker.nextOrdinal(), rawString, Some(NameInfo(thisSuite.suiteName, Some(thisSuite.getClass.getName), testName))))
    }
  }
  
  def tags = new java.util.HashMap[String, java.util.Set[String]]()
  
  private def getSpecTestNames(theSpec: Specification): java.util.Set[String] = {
    val ownTestNames = theSpec.systems.flatMap(getSusTestNames(_, Stack[String]())).toSet 
    val subTestNames = theSpec.subSpecifications.flatMap(getSpecTestNames(_)).toSet
    ownTestNames ++ subTestNames
  }
  
  def testNames = getSpecTestNames(spec)
  
  private def getSusTestNames(sus: Sus, scopeStack: Stack[String]): Set[String] = {
    val sysDesc = sus.description + " " + sus.verb
    if (scopeStack.isEmpty)
      scopeStack.push(sysDesc) 
    else 
      scopeStack.push(scopeStack.head + " " + sysDesc)
    
    val testNames = sus.examples.flatMap(getExampleTestNames(_, scopeStack))
    
    scopeStack.pop()
    
    testNames.toSet
  }
  
  private def getExampleTestNames(example: Example, scopeStack: Stack[String]): Set[String] = {
    if (example.hasSubExamples) {
      if (scopeStack.isEmpty)
      scopeStack.push(example.description) 
    else 
      scopeStack.push(scopeStack.head + " " + example.description)
      
      val testNames = example.examples.flatMap(getExampleTestNames(_, scopeStack))
      
      scopeStack.pop()
      testNames.toSet
    }
    else 
      Set(getTestName(scopeStack, example.description))
  }
  
  private def getSpecTestCount(theSpec: Specification, filter: JFilter): Int = {
    theSpec.systems.foldLeft(0)(_ + getSusTestCount(_, filter, Stack[String]())) +
    theSpec.subSpecifications.foldLeft(0)(_ + getSpecTestCount(_, filter))
  }
  
  private def getSusTestCount(sus: Sus, filter: JFilter, scopeStack: Stack[String]): Int = {
    val sysDesc = sus.description + " " + sus.verb
    if (scopeStack.isEmpty)
      scopeStack.push(sysDesc) 
    else 
      scopeStack.push(scopeStack.head + " " + sysDesc)
      
    val count = sus.examples.foldLeft(0)(_ + getExampleTestCount(_, filter, scopeStack))
    
    scopeStack.pop()
    
    count
  }
  
  private def getExampleTestCount(example: Example, filter: JFilter, scopeStack: Stack[String]): Int = {
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
      val filterResultArr = filter.doFilter(testName, tags, suiteId)
      val filterExample = filterResultArr(0)
      if (!filterExample)
        1
      else
        0
    }
  }
  
  private def getTestName(scopeStack: Stack[String], testText: String) = {
    if (scopeStack.isEmpty)
      testText
    else // the scopeStack.length check is to make sure for the first scope "", there's no need for the space to concat.
      scopeStack.head + " " + testText 
  }
  
  def expectedTestCount(filter: JFilter) = getSpecTestCount(spec, filter)
  
  def rerunner = suiteId
  
  def styleName = "org.specs.Specification"
  
  def suiteName = specificationClass.getSimpleName
  
  def suiteId = specificationClass.getName
  
}