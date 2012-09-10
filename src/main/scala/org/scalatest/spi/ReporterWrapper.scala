package org.scalatest

import org.scalatest.events._

class ReporterWrapper(dispatch: Reporter) extends JReporter {

  def createMotionToSuppress = MotionToSuppress
  
  def createIndentedText(formattedText: String, rawText: String, indentationLevel: Int) = IndentedText(formattedText, rawText, indentationLevel)
  
  private def stringOpt(value: String) = if (value == null) None else Some(value)
  
  private def throwableOpt(value: Throwable) = if (value == null) None else Some(value)
  
  private def formatterOpt(value: JFormatter): Option[Formatter] = 
    value match {
      case MotionToSuppress => Some(MotionToSuppress)
      case it: IndentedText => Some(it)
    }
  
  private def locationOpt(value: JLocation): Option[Location] = 
    if (value == null)
      None
    else
      value match {
        case topOfClass: TopOfClass => Some(topOfClass)
        case topOfMethod: TopOfMethod => Some(topOfMethod)
        case lineInFile: LineInFile => Some(lineInFile)
        case SeeStackDepthException => Some(SeeStackDepthException)
        case _ => throw new IllegalArgumentException("Only built-in location is supported.")
      }
  
  private def anyOpt(value: Object) = if (value == null) None else Some(value)
  
  private def durationOpt(value: Long) = if (value < 0) None else Some(value)
  
  implicit def jOrdinalToOrdinal(value: JOrdinal): Ordinal = {
    value match {
      case o: Ordinal => o
      case _ => throw new IllegalArgumentException("Only org.scalatest.Ordinal is supported.")
    }
  }
  
  def jRecordableEventToRecordableEvent(value: JRecordableEvent): RecordableEvent = {
    value match {
      case re: RecordableEvent => re
      case _ => throw new IllegalArgumentException("Only org.scalatest.event.RecordableEvent is supported.")
    }
  }
  
  private def nameInfoOpt(suiteName: String, suiteId: String, suiteClassName: String, testName: String): Option[NameInfo] = 
    if (suiteName != null || suiteId != null || suiteClassName != null)
      Some(NameInfo(suiteName, suiteId, stringOpt(suiteClassName), None, if (testName == null) None else Some(TestNameInfo(testName, None))))
    else
      None
      
  def createInfoProvided(ordinal: JOrdinal, message: String, suiteName: String, suiteId: String, suiteClassName: String, testName: String, 
                         throwable: Throwable, formatter: JFormatter, location: JLocation, payload: Object, threadName: String, timeStamp: Long) = 
    InfoProvided (
      ordinal,
      message,
      nameInfoOpt(suiteName, suiteId, suiteClassName, testName),
      throwableOpt(throwable),
      formatterOpt(formatter),
      locationOpt(location),
      anyOpt(payload),
      threadName,
      timeStamp
    )
  
  def createMarkupProvided(ordinal: JOrdinal, text: String, suiteName: String, suiteId: String, suiteClassName: String, testName: String,
                           formatter: JFormatter, location: JLocation, payload: Object, threadName: String, timeStamp: Long) = 
    MarkupProvided (
      ordinal,
      text,
      nameInfoOpt(suiteName, suiteId, suiteClassName, testName),
      formatterOpt(formatter),
      locationOpt(location),
      anyOpt(payload),
      threadName,
      timeStamp)
  
  def createTopOfClass(className: String) = TopOfClass(className)
  
  def createTopOfMethod(className: String, methodId: String) = TopOfMethod(className, methodId)
  
  def createLineInFile(lineNumber: Int, fileName: String) = LineInFile(lineNumber, fileName)
  
  def createSeeStackDepthException = SeeStackDepthException
  
  def fireTestStarting(ordinal: JOrdinal, suiteName: String, suiteId: String, suiteClassName: String, testName: String, testText: String, formatter: JFormatter,
                       location: JLocation, rerunner: String, payload: Object, threadName: String, timeStamp: Long) {
    dispatch(
      TestStarting (
        ordinal,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None,
        testName,
        testText,
        None,
        formatterOpt(formatter),
        locationOpt(location),
        stringOpt(rerunner),
        anyOpt(payload),
        threadName,
        timeStamp
      )
    )
  }
  
  def fireTestSucceeded(ordinal: JOrdinal, suiteName: String, suiteId: String, suiteClassName: String, testName: String, testText: String, recordedEvents: Array[JRecordableEvent], 
                        duration: Long, formatter: JFormatter, location: JLocation, rerunner: String, payload: Object, threadName: String, timeStamp: Long) {
    dispatch (
      TestSucceeded (
        ordinal,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None, 
        testName,
        testText,
        None,
        recordedEvents.map(jRecordableEventToRecordableEvent(_)).toIndexedSeq, 
        durationOpt(duration),
        formatterOpt(formatter),
        locationOpt(location),
        stringOpt(rerunner),
        anyOpt(payload),
        threadName,
        timeStamp
      )    
    )
  }
  
  def fireTestFailed(ordinal: JOrdinal, message: String, suiteName: String, suiteId: String, suiteClassName: String, testName: String, testText: String, 
                     recordedEvents: Array[JRecordableEvent], throwable: Throwable, duration: Long, formatter: JFormatter, location: JLocation, rerunner: String, 
                     payload: Object, threadName: String, timeStamp: Long) {
    dispatch(
      TestFailed (
        ordinal,
        message,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None,
        testName,
        testText,
        None,
        recordedEvents.map(jRecordableEventToRecordableEvent(_)).toIndexedSeq, 
        throwableOpt(throwable),
        durationOpt(duration),
        formatterOpt(formatter),
        locationOpt(location),
        stringOpt(rerunner),
        anyOpt(payload),
        threadName,
        timeStamp
      )    
    )
  }
  
  def fireTestIgnored(ordinal: JOrdinal, suiteName: String, suiteId: String, suiteClassName: String, testName: String, testText: String, formatter: JFormatter, location: JLocation, 
                      payload: Object, threadName: String, timeStamp: Long) {
    dispatch(
      TestIgnored (
        ordinal,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None,
        testName,
        testText,
        None,
        formatterOpt(formatter),
        locationOpt(location),
        anyOpt(payload),
        threadName,
        timeStamp
      )    
    )
  }
  
  def fireTestPending(ordinal: JOrdinal, suiteName: String, suiteId: String, suiteClassName: String, testName: String, testText: String, recordedEvents: Array[JRecordableEvent], 
                      duration: Long, formatter: JFormatter, location: JLocation, payload: Object, threadName: String, timeStamp: Long) {
    dispatch(
      TestPending (
        ordinal,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None,
        testName,
        testText,
        None,
        recordedEvents.map(jRecordableEventToRecordableEvent(_)).toIndexedSeq, 
        durationOpt(duration),
        formatterOpt(formatter),
        locationOpt(location),
        anyOpt(payload),
        threadName,
        timeStamp
      )    
    )
  }
  
  def fireTestCanceled(ordinal: JOrdinal, message: String, suiteName: String, suiteId: String, suiteClassName: String, testName: String, testText: String, recordedEvents: Array[JRecordableEvent], 
                       throwable: Throwable, duration: Long, formatter: JFormatter, location: JLocation, payload: Object, threadName: String, timeStamp: Long) {
    dispatch(
      TestCanceled (
        ordinal,
        message,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None,
        testName,
        testText,
        None,
        recordedEvents.map(jRecordableEventToRecordableEvent(_)).toIndexedSeq, 
        throwableOpt(throwable),
        durationOpt(duration),
        formatterOpt(formatter),
        locationOpt(location),
        anyOpt(payload),
        threadName,
        timeStamp
      )
    )
  }
  
  def fireSuiteStarting(ordinal: JOrdinal, suiteName: String, suiteId: String, suiteClassName: String, formatter: JFormatter, location: JLocation, rerunner: String, payload: Object,
                        threadName: String, timeStamp: Long) {
    dispatch(
      SuiteStarting (
        ordinal,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None,
        formatterOpt(formatter),
        locationOpt(location),
        stringOpt(rerunner),
        anyOpt(payload),
        threadName,
        timeStamp
      )
    )
  }
  
  def fireSuiteCompleted(ordinal: JOrdinal, suiteName: String, suiteId: String, suiteClassName: String, duration: Long, formatter: JFormatter, location: JLocation, rerunner: String,
                         payload: Object, threadName: String, timeStamp: Long) {
    dispatch(
      SuiteCompleted (
        ordinal,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None,
        durationOpt(duration),
        formatterOpt(formatter),
        locationOpt(location),
        stringOpt(rerunner),
        anyOpt(payload),
        threadName,
        timeStamp
      )    
    )
  }
  
  def fireSuiteAborted(ordinal: JOrdinal, message: String, suiteName: String, suiteId: String, suiteClassName: String, throwable: Throwable, duration: Long, formatter: JFormatter,
                       location: JLocation, rerunner: String, payload: Object, threadName: String, timeStamp: Long) {
    dispatch(
      SuiteAborted (
        ordinal,
        message,
        suiteName,
        suiteId,
        stringOpt(suiteClassName),
        None, 
        throwableOpt(throwable),
        durationOpt(duration),
        formatterOpt(formatter),
        locationOpt(location),
        stringOpt(rerunner),
        anyOpt(payload),
        threadName,
        timeStamp
      )    
    )
  }
  
  def fireInfoProvided(ordinal: JOrdinal, message: String, suiteName: String, suiteId: String, suiteClassName: String, testName: String, 
                       throwable: Throwable, formatter: JFormatter, location: JLocation, payload: Object, threadName: String, timeStamp: Long) {
    dispatch(
      InfoProvided (
        ordinal,
        message,
        nameInfoOpt(suiteName, suiteId, suiteClassName, testName),
        throwableOpt(throwable),
        formatterOpt(formatter),
        locationOpt(location),
        anyOpt(payload),
        threadName,
        timeStamp
      )
    )
  }
  
  def fireMarkupProvided(ordinal: JOrdinal, text: String, suiteName: String, suiteId: String, suiteClassName: String, testName: String,
                         formatter: JFormatter, location: JLocation, payload: Object, threadName: String, timeStamp: Long) { 
    dispatch(
      MarkupProvided (
        ordinal,
        text,
        nameInfoOpt(suiteName, suiteId, suiteClassName, testName),
        formatterOpt(formatter),
        locationOpt(location),
        anyOpt(payload),
        threadName,
        timeStamp
      )
    )
  }
}