package org.scalatest.spi

import org.scalatest.{JFilter, Filter}
import collection.JavaConversions._

class FilterWrapper(filter: Filter) extends JFilter {

  private def convertMap(tags: java.util.Map[String, java.util.Set[String]]): Map[String, Set[String]] = {
    val imTestTags = new collection.mutable.HashMap[String, Set[String]]()
    for ((testName, tagSet) <- tags.toMap) 
      imTestTags += ((testName, tagSet.toSet))
    imTestTags.toMap
  }
  
  def doFilter(testNames: java.util.Set[String], tags: java.util.Map[String, java.util.Set[String]], suiteId: String) = {
    val javaMap = new java.util.HashMap[String, java.lang.Boolean]()
    val scalaMap = Map.empty ++ filter(testNames.toSet, convertMap(tags), suiteId)
    for ((testName, filtered) <- scalaMap)
      javaMap.put(testName, filtered)
    javaMap
  }
    
  def doFilter(testName: String, tags: java.util.Map[String, java.util.Set[String]], suiteId: String) = { 
    val (filtered, ignored) = filter(testName, convertMap(tags), suiteId)
    Array(filtered, ignored)
  }
  
  def runnableTestCount(testNames: java.util.Set[String], testTags: java.util.Map[String, java.util.Set[String]], suiteId: String): Int = 
    filter.runnableTestCount(testNames.toSet, convertMap(testTags), suiteId)  
}