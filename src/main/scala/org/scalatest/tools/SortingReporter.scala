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
  private val waitingMap = new collection.mutable.HashMap[String, Event]()
  private val expectedInfoMap = new collection.mutable.HashMap[String, Int]()
  private val waitingInfoMap = new collection.mutable.HashMap[String, List[InfoProvided]]()
  
  override def apply(event: Event) {
    
    event match {
      case event: TestStarting => increaseStartedCount(event)
      case event: TestIgnored => increaseStartedCount(event)
      case event: TestSucceeded => handleDoneEvent(event, event.testName)
      case event: TestFailed => handleDoneEvent(event, event.testName)
      case event: TestPending => handleDoneEvent(event, event.testName)
      case event: InfoProvided => handleInfoProvided(event)
        
    }
  }
  
  private def store(event: Event) {
    synchronized {
      eventBuffer += event
    }
  }
  
  private def handleInfoProvided(infoProvided: InfoProvided) {
    synchronized {
      if (allStarted) {
        infoProvided.nameInfo match {
          case Some(nameInfo) =>
            nameInfo.testName match {
              case Some(testName) => 
                expectedInfoMap.get(testName) match {
                  case Some(expectedInfoCount) =>
                    val currentInfoList = waitingInfoMap(testName)
                    val newInfoList = infoProvided :: currentInfoList
                    waitingInfoMap.put(testName, newInfoList)
                    if (newInfoList.size == expectedInfoCount) {
                      doneMap.put(testName, waitingMap(testName))
                      fireReadyEvents()
                    }
                  case None =>
                    store(infoProvided)
                }
              case None => 
                store(infoProvided)
            }
          case None => 
            store(infoProvided)
        }
      }
      else
        store(infoProvided)
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
        println("###" + eventArray.toList.map(e => e.getClass.getSimpleName + "(" + e.ordinal.toList + ")"))
        eventBuffer = new ListBuffer[Event]() ++ eventArray
        allStarted = true
      }
    }
  }
  
  private def getInfoCount(doneEvent: Event) = {
    doneEvent match {
      case event: TestSucceeded => event.infoCount
      case event: TestFailed => event.infoCount
      case event: TestPending => event.infoCount
    }
  }
  
  private def handleDoneEvent(doneEvent: Event, testName: String) {
    synchronized {
      val infoCount = getInfoCount(doneEvent)
      if (infoCount == 0) {
        doneMap.put(testName, doneEvent)
        completedCount += 1
      }
      else {
        waitingMap.put(testName, doneEvent)
        expectedInfoMap.put(testName, infoCount)
        waitingInfoMap.put(testName, List.empty[InfoProvided])
      }
      fireReadyEvents()            
    }
  }
  
  private def fireReadyEvents() {
    if (allStarted) {
      if (completedCount == testCount) {
        eventBuffer.foreach { e =>
          e match {
            case testStarting: TestStarting => 
              reporter(e)
              val testName = testStarting.testName
              reporter(doneMap(testName))
              waitingInfoMap.get(testName) match {
                case Some(infoList) => 
                  infoList.reverse.foreach(reporter(_))
                case None => 
              }
            case _ =>
              reporter(e)
          }
        }
        eventBuffer.clear()
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
              val testName = testStarting.testName
              reporter(doneMap(testName))
              waitingInfoMap.get(testName) match {
                case Some(infoList) => 
                  infoList.reverse.foreach(reporter(_))
                case None => 
              }
            case _ =>
              reporter(e)
          }
        }
      }
    }
  }
  
  override def dispose() = propagateDispose(reporter)
}