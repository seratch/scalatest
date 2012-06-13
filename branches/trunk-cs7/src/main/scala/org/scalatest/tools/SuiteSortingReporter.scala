package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

class SuiteSortingReporter(dispatch: Reporter) extends ResourcefulReporter {

  case class Slot(suiteId: String, var doneEvent: Option[Event], var testSortingReporter: Option[TestSortingReporter])

  @volatile private var slotList = new ListBuffer[Slot]()
  private val slotMap = collection.mutable.HashMap[String, Slot]()
  private val suiteEventMap = collection.mutable.HashMap[String, List[Event]]()

  override def apply(event: Event) {
    try {
      synchronized {
        event match {
          case suiteStarting: SuiteStarting =>
            // TODO: use better way to handle 'container' suite, add testCount in SuiteStarting may be?
            if (suiteStarting.suiteId == "org.scalatest.tools.DiscoverySuite")
              dispatch(suiteStarting)
            else {
              val slot = Slot(suiteStarting.suiteId, None, None)
              slotList += slot
              slotMap.get(suiteStarting.suiteId) match {
                case Some(slot) =>
                  throw new RuntimeException("2 SuiteStarting (" + slot.suiteId + ", " + suiteStarting.suiteId + ") having same suiteId '" + suiteStarting.suiteId + "'.")
                case None =>
                  slotMap.put(suiteStarting.suiteId, slot)
              }
              handleTestEvents(suiteStarting.suiteId, suiteStarting)
            }
          case suiteCompleted: SuiteCompleted =>
            // TODO: use better way to handle 'container' suite, add testCount in SuiteStarting may be?
            if (suiteCompleted.suiteId == "org.scalatest.tools.DiscoverySuite")
              dispatch(suiteCompleted)
            else {
              val slot = slotMap(suiteCompleted.suiteId)
              slot.doneEvent = Some(suiteCompleted)
            }
          case suiteAborted: SuiteAborted =>
            // TODO: use better way to handle 'container' suite, add testCount in SuiteStarting may be?
            if (suiteAborted.suiteId == "org.scalatest.tools.DiscoverySuite")
              dispatch(suiteAborted)
            else {
              val slot = slotMap(suiteAborted.suiteId)
              slot.doneEvent = Some(suiteAborted)
            }
          case testStarting: TestStarting =>
            handleTestEvents(testStarting.suiteId, testStarting)
          case testIgnored: TestIgnored =>
            handleTestEvents(testIgnored.suiteId, testIgnored)
          case testSucceeded: TestSucceeded =>
            handleTestEvents(testSucceeded.suiteId, testSucceeded)
          case testFailed: TestFailed =>
            handleTestEvents(testFailed.suiteId, testFailed)
          case testPending: TestPending =>
            handleTestEvents(testPending.suiteId, testPending)
          case testCanceled: TestCanceled =>
            handleTestEvents(testCanceled.suiteId, testCanceled)
          case infoProvided: InfoProvided =>
            infoProvided.nameInfo match {
              case Some(nameInfo) =>
                handleTestEvents(nameInfo.suiteID, infoProvided)
              case None => // Under what condition will reach here?
                dispatch(infoProvided)
            }
          case markupProvided: MarkupProvided =>
            markupProvided.nameInfo match {
              case Some(nameInfo) =>
                handleTestEvents(nameInfo.suiteID, markupProvided)
              case None => // Under what condition will reach here?
                dispatch(markupProvided)
            }
          case scopeOpened: ScopeOpened =>
            handleTestEvents(scopeOpened.nameInfo.suiteID, scopeOpened)
          case scopeClosed: ScopeClosed =>
            handleTestEvents(scopeClosed.nameInfo.suiteID, scopeClosed)
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

  private[scalatest] def setTestSortingReporter(suiteId: String, testSortingReporter: TestSortingReporter) {
    val slot = slotMap(suiteId)
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