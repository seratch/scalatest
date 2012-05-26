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
  
  @tailrec
  private def fireReadyEvents() {
    if (slotList.size > 0) {
      val head = slotList.head
      suiteEventMap.get(head.suiteId) match {
        case Some(eventList) => 
          eventList.foreach(dispatch(_))
          suiteEventMap.put(head.suiteId, List.empty[Event])
        case None =>
          println("###Unable to event list from event map: " + head.suiteId)
      }
      head.testSortingReporter match {
        case Some(testSortingReporter) => 
          if (head.doneEvent.isDefined && testSortingReporter.finished) {
            dispatch(head.doneEvent.get)
            slotList = slotList.tail
            fireReadyEvents()
          }
        case None =>
          // In case for non-parallel suite
          if (head.doneEvent.isDefined) {
            dispatch(head.doneEvent.get)
            slotList = slotList.tail
            fireReadyEvents()
          }
      }
    }
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