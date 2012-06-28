package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import org.scalatest.time.Span

private[scalatest] class SuiteSortingReporter(dispatch: Reporter) extends ResourcefulReporter with DistributedSuiteSorter {

  case class Slot(suiteId: String, doneEvent: Option[Event], testSortingReporter: Option[TestSortingReporter], testsCompleted: Boolean)

  @volatile private var slotList = new ListBuffer[Slot]()
  private val slotMap = collection.mutable.HashMap[String, Slot]()
  private val suiteEventMap = collection.mutable.HashMap[String, List[Event]]()

  override def apply(event: Event) {
    try {
      synchronized {
        event match {
          case suiteStarting: SuiteStarting =>
            val slot = Slot(suiteStarting.suiteId, None, None, false)
            slotList += slot
            slotMap.get(suiteStarting.suiteId) match {
              case Some(slot) =>
                throw new RuntimeException("2 SuiteStarting (" + slot.suiteId + ", " + suiteStarting.suiteId + ") having same suiteId '" + suiteStarting.suiteId + "'.")
              case None =>
                slotMap.put(suiteStarting.suiteId, slot)
            }
            handleTestEvents(suiteStarting.suiteId, suiteStarting)

          case suiteCompleted: SuiteCompleted =>
            handleSuiteEvents(suiteCompleted.suiteId, suiteCompleted)
          case suiteAborted: SuiteAborted =>
            handleSuiteEvents(suiteAborted.suiteId, suiteAborted)
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
          case _ =>
            dispatch(event)  // Just dispatch it if got unexpected event.
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

  private def handleSuiteEvents(suiteId: String, event: Event) {
    val slot = slotMap(suiteId)
    //slot.doneEvent = Some(event)
    val newSlot = slot.copy(doneEvent = Some(event))
    slotMap.put(suiteId, newSlot)
    val slotIdx = slotList.indexOf(slot)
    if (slotIdx >= 0)
      slotList.update(slotIdx, newSlot)
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
        slot.doneEvent.isDefined && slot.testsCompleted
      case None =>
        slot.doneEvent.isDefined
    }
  }

  private def fireReadySuiteEvents(remainingSlotList: ListBuffer[Slot]): ListBuffer[Slot] = {
    val (done, pending) = remainingSlotList.span(isDone(_))
    done.foreach {
      slot =>
        fireSuiteEvents(slot.suiteId)
        dispatch(slot.doneEvent.get)
    }
    pending
  }
  
  def completedTests(suiteId: String) {
    val slot = slotMap(suiteId)
    val newSlot = slot.copy(testsCompleted = true)
    slotMap.put(suiteId, newSlot)
    val slotIdx = slotList.indexOf(slot)
    if (slotIdx >= 0)
      slotList.update(slotIdx, newSlot)
    fireReadyEvents()
  }
  
  def distributingTests(suiteId: String, timeout: Span, testCount: Int) = {
    val testSortingReporter = new TestSortingReporter(suiteId, this, timeout, testCount, Some(this))
    val slot = slotMap(suiteId)
    val newSlot = slot.copy(testSortingReporter = Some(testSortingReporter))
    slotMap.put(suiteId, newSlot)
    val slotIdx = slotList.indexOf(slot)
    if (slotIdx >= 0)
      slotList.update(slotIdx, newSlot)
    testSortingReporter
  }
  
  def getDistributedTestSorter(suiteId: String) = slotMap(suiteId).testSortingReporter.get

  override def dispose() = {
    try {
      fireReadyEvents()
    }
    catch {
      case e: Exception =>
        val stringToPrint = Resources("reporterDisposeThrew")
        System.err.println(stringToPrint)
        e.printStackTrace(System.err)
    }
  }
}