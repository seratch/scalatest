package org.scalatest.tools

import org.scalatest._
import org.scalatest.events._
import DispatchReporter.propagateDispose
import scala.collection.mutable.ListBuffer
import scala.util.Sorting
import scala.annotation.tailrec

private[scalatest] class TestSortingReporter(dispatch: Reporter, testCount: Int, structure: Option[SuiteStructure.Branch]) extends ResourcefulReporter {
  
  case class Slot(node: Option[SuiteStructure.Node], var event: Option[Event], var doneEvent: Option[Event], var infoCount: Int, var infoList: List[Event], var ready: Boolean) extends Ordered[Slot] {
    def compare(that: Slot) = event.compare(that.event)
  }
  
  private val slotMap = new collection.mutable.HashMap[String, Slot]()
  private val startingBuffer = new ListBuffer[Event]()
  private var startedCount = 0
  private var totalSlotCount = 0
  private var firedSlotCount = 0
  
  @volatile
  private var linearizedStructure = 
    structure match {
      case Some(structure) => 
        linearizeStructure(structure)
      case None =>
        null
    }
 
  private def linearizeStructure(root: SuiteStructure.Branch): List[Slot] = {
    @tailrec
    def serializeAcc(nodeList: List[SuiteStructure.Node], accList: List[Slot]): List[Slot] = {
      nodeList match {
        case head :: tail => 
          val headSlot = Slot(Some(head), None, None, 0, List.empty[Event], false)
          head match {
            case branch: SuiteStructure.Branch =>
              serializeAcc(branch.subNodes ::: tail, headSlot :: accList)
            case testLeaf: SuiteStructure.TestLeaf =>
              slotMap.put(testLeaf.testName, headSlot)
              serializeAcc(tail, headSlot :: accList)
            case _ =>
              serializeAcc(tail, headSlot :: accList)
          }
          
        case Nil => accList
      }
    }
    val slotList = serializeAcc(root.subNodes, List.empty[Slot]).reverse
    totalSlotCount = slotList.size
    slotList
  }
  
  def finished = {
    totalSlotCount == firedSlotCount
  }
  
  override def apply(event: Event) {
    synchronized {
      if (linearizedStructure == null) {
        startingBuffer += event
        if (event.isInstanceOf[TestStarting])
          startedCount += 1
        if (startedCount == testCount) {
          val startingArray = startingBuffer.toArray
          Sorting.quickSort(startingArray)
          startingBuffer.clear()
          startingBuffer ++= startingArray
          linearizedStructure = startingBuffer.map { e =>
            e match {
              case event: TestStarting => 
                // create a slot here
                val slot = Slot(None, Some(event), None, 0, List.empty[Event], false)
                slotMap.put(event.testName, slot)
                slot
              case event: TestIgnored => 
                // create a slot here, with ready status
                Slot(None, Some(event), None, 0, List.empty[Event], true)
              case event: TestSucceeded => 
                // find a previously created slot, set it done event and wait for test info events, if any.
                val slot = slotMap(event.testName)
                slot.doneEvent = Some(event)
                if (event.infoCount == 0)
                  slot.ready = true
                else
                  slot.infoCount = event.infoCount
                slot
              case event: TestFailed => 
                // find a previously created slot, set it done event and wait for test info events, if any.
                val slot = slotMap(event.testName)
                slot.doneEvent = Some(event)
                if (event.infoCount == 0)
                  slot.ready = true
                else
                  slot.infoCount = event.infoCount
                slot
              case event: TestPending => 
                // find a previously created slot, set it done event and wait for test info events, if any.
                val slot = slotMap(event.testName)
                slot.doneEvent = Some(event)
                if (event.infoCount == 0)
                  slot.ready = true
                else
                  slot.infoCount = event.infoCount
                slot
              case event: InfoProvided => // Check if it is a test info event (from the name info probably)
                event.nameInfo match {
                  case Some(nameInfo) => 
                    nameInfo.testName match {
                      case Some(testName) => 
                        // Test's info event
                        val slot = slotMap(testName)
                        slot.infoList = slot.infoList ::: List(event)
                        if (slot.infoList.size == slot.infoCount)
                          slot.ready = true
                        slot
                      case None => 
                        Slot(None, Some(event), None, 0, List.empty[Event], true)
                    }
                  case None => 
                    Slot(None, Some(event), None, 0, List.empty[Event], true)
                }
            }
          }.toList
          totalSlotCount = linearizedStructure.size
        }
      }
      else {
        // Fill in the slot created from structure
        event match {
          case event: TestStarting => 
            val slot = slotMap(event.testName)
            slot.event = Some(event)
          case event: TestIgnored =>
            val slot = slotMap(event.testName)
            slot.event = Some(event)
            slot.ready = true
          case event: TestSucceeded => 
            val slot = slotMap(event.testName)
            slot.doneEvent = Some(event)
            if (event.infoCount == 0)
              slot.ready = true
            else
              slot.infoCount = event.infoCount
          case event: TestFailed =>
            val slot = slotMap(event.testName)
            slot.doneEvent = Some(event)
            if (event.infoCount == 0)
              slot.ready = true
            else
              slot.infoCount = event.infoCount
          case event: TestPending => 
            val slot = slotMap(event.testName)
            slot.doneEvent = Some(event)
            if (event.infoCount == 0)
              slot.ready = true
            else
              slot.infoCount = event.infoCount
          case event: InfoProvided => 
            event.nameInfo match {
              case Some(nameInfo) => 
                nameInfo.testName match {
                  case Some(testName) => 
                    // Test's info event
                    val slot = slotMap(testName)
                    slot.infoList = slot.infoList ::: List(event)
                    if (slot.infoList.size == slot.infoCount)
                      slot.ready = true
                  case None => 
                    handleNoneTestInfo(event)
                }
              case None => 
                handleNoneTestInfo(event)
            }
        }
      }
      fireReadyEvents()
    }
  }
  
  private def handleNoneTestInfo(event: InfoProvided) {
    // TODO, to check if it'll work correctly if there's info with same message running in parallel.
    linearizedStructure.find { slot => 
      if (slot.ready)
        false
      else {
        slot.node match {
          case Some(node) => 
            node match {
              case descBranch: SuiteStructure.DescriptionBranch if descBranch.descriptionText == event.message => true
              case _ => false
            }
          case None => false
        }
      }
    } match {
        case Some(slot) => 
          // matching info leaf found
          slot.event = Some(event)
          slot.ready = true
        case None => 
          // Should not happen, if it does happen, just fire the event.
          dispatch(event)
      }
  }
  
  private[tools] def fireReadyEvents() {
    if (linearizedStructure != null) {
      val readySlots = linearizedStructure.takeWhile(_.ready)
      linearizedStructure = linearizedStructure.drop(readySlots.size)
      readySlots.foreach { slot => 
        dispatch(slot.event.get)  // Should always has event
        slot.doneEvent match { // TestIgnored and InfoProvided doesn't have doneEvent
          case Some(doneEvent) => dispatch(doneEvent)
          case None =>
        }
        slot.infoList.foreach(dispatch(_))
        firedSlotCount += 1
      }
    }
  }
  
  override def dispose() = propagateDispose(dispatch)
}