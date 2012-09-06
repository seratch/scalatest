package org.scalatest

import org.scalatest.events._

class ReporterWrapper(reporter: Reporter) extends JReporter {

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
        case _ => throw new IllegalArgumentException("Only built-in location is supported, you cannot pass in custom type of JLocation.")
      }
  
  private def anyOpt(value: Object) = if (value == null) None else Some(value)
  
  implicit def jOrdinalToOrdinal(value: JOrdinal): Ordinal = {
    value match {
      case o: Ordinal => o
      case _ => throw new IllegalArgumentException("Only org.scalatest.Ordinal is supported, you cannot pass in other type of JOrdinal.")
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
                           formatter: JFormatter, location: JLocation, payload: Object, threadName: String, timeStamp: Long) = {
    null
  }
  
}