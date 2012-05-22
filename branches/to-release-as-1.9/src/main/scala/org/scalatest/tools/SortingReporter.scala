package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.util.Sorting

private[scalatest] class SortingReporter(reporter: Reporter, testCount: Int) extends ResourcefulReporter {

  private var eventBuffer = new ListBuffer[Event]()
  private var startedCount = 0
  private var completedCount = 0
  private var allStarted: Boolean = false
  private val doneMap = new collection.mutable.HashMap[String, Event]()
  
  override def apply(event: Event) {
    
    event match {
      case event: TestStarting => increaseStartedCount(event)
      case event: TestIgnored => increaseStartedCount(event)
      case event: TestSucceeded => handleDoneEvent(event, event.testName)
      case event: TestFailed => handleDoneEvent(event, event.testName)
      case event: TestPending => handleDoneEvent(event, event.testName)
      case _ => 
        eventBuffer += event
    }
  }
  
  private def increaseStartedCount(event: Event) {
    synchronized {
      eventBuffer += event
      startedCount += 1
      if (event.isInstanceOf[TestIgnored])
        completedCount += 1
      if (startedCount == testCount) {
        val eventArray = eventBuffer.toArray
        Sorting.quickSort(eventArray)
        eventBuffer = new ListBuffer[Event]() ++ eventArray
        allStarted = true
      }
    }
  }
  
  private def handleDoneEvent(doneEvent: Event, testName: String) {
    synchronized {
      doneMap.put(testName, doneEvent)
      completedCount += 1
      if (allStarted) {
        if (completedCount == testCount) {
          eventBuffer.foreach { e =>
            e match {
              case testStarting: TestStarting => 
                reporter(e)
                reporter(doneMap(testStarting.testName))
              case _ =>
                reporter(e)
            }
          }
          eventBuffer = new ListBuffer[Event]()
        }
        else {
          val readyBuffer = eventBuffer.takeWhile { e => 
            e match {
              case TestStarting(ordinal,  suiteName, suiteClassName, theTestName, formatter,
                                rerunner, payload, threadName, timeStamp) if !doneMap.contains(theTestName) =>
                false
              case _ =>
                true
            }
          }
          eventBuffer = eventBuffer.drop(readyBuffer.size)
          readyBuffer.foreach { e => 
            e match {
              case testStarting: TestStarting => 
                reporter(e)
                reporter(doneMap(testStarting.testName))
              case _ =>
                reporter(e)
            }
          }
        }
      }      
    }
  }
  
  override def dispose() = propagateDispose(reporter)
}