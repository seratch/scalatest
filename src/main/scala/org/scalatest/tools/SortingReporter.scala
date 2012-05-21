package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.util.Sorting

private[scalatest] class SortingReporter(reporter: Reporter, testCount: Int) extends ResourcefulReporter {

  private val eventBuffer = new ListBuffer[Event]()
  private var testCompleted = 0
  
  override def apply(event: Event) {
    eventBuffer += event
    event match {
      case event: TestFailed => increaseCountAndCheckToSend()
      case event: TestSucceeded => increaseCountAndCheckToSend()
      case event: TestIgnored => increaseCountAndCheckToSend()
      case event: TestPending => increaseCountAndCheckToSend()
      case _ => 
    }
  }
  
  private def increaseCountAndCheckToSend() {
    synchronized {
      testCompleted += 1
      if (testCompleted == testCount) {
        // sort and dispatch all events
        val eventArray = eventBuffer.toArray
        Sorting.quickSort(eventArray)
        eventArray.foreach(reporter(_))
      }
    }
  }
  
  override def dispose() = propagateDispose(reporter)
}