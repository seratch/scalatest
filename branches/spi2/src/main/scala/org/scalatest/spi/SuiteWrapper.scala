package org.scalatest.spi

import org.scalatest._
import scala.collection.JavaConversions._

private[scalatest] class SuiteWrapper(jSuite: JSuite) extends Suite {
  
  override def run(testName: Option[String], args: Args) {
    jSuite.run(if (testName.isDefined) testName.get else null, 
               new JArgs() {
                 def reporter = new ReporterWrapper(args.reporter)
                 def stopper = new StopperWrapper(args.stopper)
                 def filter = new FilterWrapper(args.filter)
                 def configMap = {
                   val javaMap = new java.util.HashMap[String, Object]()
                   for ((key, value) <- args.configMap.toMap) 
                     javaMap put (key, value.asInstanceOf[AnyRef])
                   javaMap
                 }
                 def tracker = new TrackerWrapper(args.tracker)
                 def chosenStyles = args.chosenStyles
               })
  }
  
  override def testNames: Set[String] = jSuite.testNames.toSet
  
  override def tags: Map[String, Set[String]] = {
    val mutMap = new collection.mutable.HashMap[String, Set[String]]()
    for ((testName, tagSet) <- jSuite.tags.toMap) {
      mutMap += ((testName, tagSet.toSet))
    } 
    mutMap.toMap
  }
  
  override def expectedTestCount(filter: Filter): Int = jSuite.expectedTestCount(new FilterWrapper(filter))
  
  override def rerunner: Option[String] = if (jSuite.rerunner == null) Some(jSuite.rerunner) else None
  
  override val styleName: String = jSuite.styleName
  
  override def suiteName: String = jSuite.suiteName
  
  override def suiteId: String = jSuite.suiteId  
}