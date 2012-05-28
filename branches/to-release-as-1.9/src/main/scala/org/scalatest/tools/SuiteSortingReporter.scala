package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

class SuiteSortingReporter(dispatch: Reporter) extends ResourcefulReporter {

  case class Slot(suiteId: String, var doneEvent: Option[Event], var testSortingReporter: Option[TestSortingReporter])
  
  private var slotList = new ListBuffer[Slot]()
  private val slotMap = collection.mutable.HashMap[String, Slot]()
  private val suiteEventMap = collection.mutable.HashMap[String, List[Event]]()
  
  override def apply(event: Event) {
    try {
      synchronized {
        event match {
          case suiteStarting: SuiteStarting => 
            // TODO: use better way to handle 'container' suite, add testCount in SuiteStarting may be?
            if (suiteStarting.suiteClassName.get == "org.scalatest.tools.DiscoverySuite")
              dispatch(suiteStarting)
            else {
              val slot = Slot(suiteStarting.suiteClassName.get, None, None)
              slotList += slot
              slotMap.put(suiteStarting.suiteClassName.get, slot)  // Should use suiteId here in 2.0
              handleTestEvents(suiteStarting.suiteClassName.get, suiteStarting)
            }
          case suiteCompleted: SuiteCompleted =>
            // TODO: use better way to handle 'container' suite, add testCount in SuiteStarting may be?
            if (suiteCompleted.suiteClassName.get == "org.scalatest.tools.DiscoverySuite")
              dispatch(suiteCompleted)
            else {
              val slot = slotMap(suiteCompleted.suiteClassName.get)
              slot.doneEvent = Some(suiteCompleted)
            }
          case suiteAborted: SuiteAborted => 
            // TODO: use better way to handle 'container' suite, add testCount in SuiteStarting may be?
            if (suiteAborted.suiteClassName.get == "org.scalatest.tools.DiscoverySuite")
              dispatch(suiteAborted)
            else {
              val slot = slotMap(suiteAborted.suiteClassName.get)
              slot.doneEvent = Some(suiteAborted)
            }
          case testStarting: TestStarting =>
            handleTestEvents(testStarting.suiteClassName.get, testStarting)
          case testIgnored: TestIgnored => 
            handleTestEvents(testIgnored.suiteClassName.get, testIgnored)
          case testSucceeded: TestSucceeded => 
            handleTestEvents(testSucceeded.suiteClassName.get, testSucceeded)
          case testFailed: TestFailed => 
            handleTestEvents(testFailed.suiteClassName.get, testFailed)
          case testPending: TestPending => 
            handleTestEvents(testPending.suiteClassName.get, testPending)
          case infoProvided: InfoProvided => 
            infoProvided.nameInfo match {
              case Some(nameInfo) =>
                handleTestEvents(nameInfo.suiteClassName.get, infoProvided)  // should use suiteId here.
              case None => // Under what condition will reach here?
            }
          case _ => 
        }
        fireReadyEvents()
      }
    }
    catch {
      case e: Exception => 
        val stringToPrint = Resources("reporterThrew", event)
        System.err.println(stringToPrint)
        e.printStackTrace(System.err)
    }
  }
  
  private def handleTestEvents(suiteId: String, event: Event) {
    suiteEventMap.get(suiteId) match {
      case Some(eventList) => 
        suiteEventMap.put(suiteId, eventList ::: List(event))
      case None => 
        suiteEventMap.put(suiteId, List(event))
    }
    fireReadyEvents()
  }
  
  private def fireReadyEvents() {
    if (slotList.size > 0) {
      val head = slotList.head
      fireSuiteEvents(head.suiteId)
      if (isDone(head)) {
        dispatch(head.doneEvent.get)
        slotList = fireReadySuiteEvents(slotList.tail)
      }
    }
  }
  
  private def fireSuiteEvents(suiteId: String) {
    suiteEventMap.get(suiteId) match {
      case Some(eventList) => 
        eventList.foreach(dispatch(_))
        suiteEventMap.put(suiteId, List.empty[Event])
      case None =>
        // Unable to get event list from map, shouldn't happen
    }
  }
  
  private def isDone(slot: Slot) = {
    slot.testSortingReporter match {
      case Some(testSortingReporter) => 
        slot.doneEvent.isDefined && testSortingReporter.finished
      case None => 
        slot.doneEvent.isDefined
    }
  }
  
  private def fireReadySuiteEvents(remainingSlotList: ListBuffer[Slot]): ListBuffer[Slot] = { 
    val (done, pending) = remainingSlotList.span(isDone(_))
    done.foreach { slot => 
      fireSuiteEvents(slot.suiteId)
      dispatch(slot.doneEvent.get)
    }
    pending
  }
  
  private[scalatest] def setTestSortingReporter(suite: Suite, testSortingReporter: TestSortingReporter) {
    // TODO: In 2.0, use suiteId here.
    val slot = slotMap(suite.getClass.getName)
    slot.testSortingReporter = Some(testSortingReporter)
  }
  
  override def dispose() = { 
    try {
      fireReadyEvents()
      //propagateDispose(dispatch)
    }
    catch {
      case e: Exception =>
        val stringToPrint = Resources("reporterDisposeThrew")
        System.err.println(stringToPrint)
        e.printStackTrace(System.err)
    }
  }
}