package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class TestSortingReporter(dispatch: Reporter, testCount: Int) extends ResourcefulReporter {
  
  case class Slot(var startEvent: Option[Event], var endEvent: Option[Event], var postEventCount: Int, var postEventList: List[Event], var ready: Boolean, var fired: Boolean)
  
  private val slotBuffer = new ListBuffer[Slot]()
  private val slotMap = new collection.mutable.HashMap[String, Slot]()  // suiteId -> slot
  
  private def getSlot(idx: Int): Slot = {
    if (idx >= slotBuffer.size) {
      while (slotBuffer.size <= idx)
        slotBuffer += Slot(None, None, 0, List.empty[Event], false, false)
    }
    slotBuffer(idx)
  }
  
  override def apply(event: Event) {
    synchronized {
      event match {
        case testStarting: TestStarting => 
          testStarting.structureIndex match {
            case Some(idx) => 
              val slot = getSlot(idx)
              slot.startEvent = Some(event)
              slotMap.put(testStarting.testName, slot)
            case None =>
              dispatch(event)
          }
        case testIgnored: TestIgnored => 
          testIgnored.structureIndex match {
            case Some(idx) => 
              val slot = getSlot(idx)
              slot.startEvent = Some(event)
              slot.ready = true
            case None =>
              dispatch(event)
          }
        case testSucceeded: TestSucceeded => 
          handleTestEndEvent(testSucceeded, testSucceeded.structureIndex, testSucceeded.postEventCount)
        case testFailed: TestFailed => 
          handleTestEndEvent(testFailed, testFailed.structureIndex, testFailed.postEventCount)
        case testPending: TestPending => 
          handleTestEndEvent(testPending, testPending.structureIndex, testPending.postEventCount)
        case testCanceled: TestCanceled =>
          handleTestEndEvent(testCanceled, testCanceled.structureIndex, testCanceled.postEventCount)
        case scopeOpened: ScopeOpened =>
          scopeOpened.structureIndex match {
            case Some(idx) => 
              val slot = getSlot(idx)
              slot.startEvent = Some(event)
              slot.ready = true
            case None => 
              dispatch(event)
          }
        case scopeClosed: ScopeClosed =>
          scopeClosed.structureIndex match {
            case Some(idx) => 
              val slot = getSlot(idx)
              slot.startEvent = Some(event)
              slot.ready = true
            case None =>
              dispatch(event)
          }
        case infoProvided: InfoProvided => 
          handleTestInfoMarkupProvided(infoProvided, infoProvided.structureIndex, infoProvided.nameInfo)
        case markupProvided: MarkupProvided =>
          handleTestInfoMarkupProvided(markupProvided, markupProvided.structureIndex, markupProvided.nameInfo)
        case _ =>
          // Should not happen, but in case it does let's just fire the event out
          dispatch(event)
      }
      fireReadyEvents()
    }
  }
  
  private def handleTestInfoMarkupProvided(event: Event, structureIndex: Option[Int], nameInfo: Option[NameInfo]) {
    nameInfo match {
      case Some(nameInfo) => 
        nameInfo.testName match {
          case Some(testNameInfo) => 
            val slot = slotMap(testNameInfo.testName)
            slot.postEventList = slot.postEventList ::: List(event)
            if (slot.postEventList.size == slot.postEventCount)
              slot.ready = true
          case None => 
            handleNonTestInfoMarkupProvided(event, structureIndex)
        }
      case None =>
        handleNonTestInfoMarkupProvided(event, structureIndex)
    }
  }
  
  private def handleNonTestInfoMarkupProvided(event: Event, structureIndex: Option[Int]) {
    structureIndex match {
      case Some(idx) => 
        val slot = getSlot(idx)
        slot.startEvent = Some(event)
        slot.ready = true
      case None =>
        // Should not happen, but if it does just fire it.
        dispatch(event)
    }
  }
  
  private def handleTestEndEvent(event: Event, structureIndex: Option[Int], postEventCount: Int) {
    structureIndex match {
      case Some(idx) =>
        val slot = getSlot(idx)
        slot.endEvent = Some(event)
        if (postEventCount == 0)
          slot.ready = true
        else
          slot.postEventCount = postEventCount
      case None => 
        dispatch(event)
    }
  }
  
  @tailrec
  private def fireReadyEvents() {
    val pending = slotBuffer.dropWhile(slot => slot.ready && slot.fired)
    if (pending.size > 0) {
      val head = pending.head
      if (head.ready) {
        dispatch(head.startEvent.get)
        head.endEvent match {
          case Some(endEvent) => 
            dispatch(endEvent)
          case None =>
        }
        head.postEventList.foreach(event => dispatch(event))
        head.fired = true
        fireReadyEvents()
      }
    }
  }
  
  override def dispose() = propagateDispose(dispatch)
}