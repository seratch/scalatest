/*
 * Copyright 2001-2011 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.tools

import org.scalatest.events._
import org.scalatest.Reporter
import org.scalatest.events.MotionToSuppress
import org.scalatest.StackDepthException

import java.io.PrintWriter
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.File
import java.util.Date
import java.text.SimpleDateFormat

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

/**
 * A <code>Reporter</code> that writes test status information in xml format
 * for use by Flex formatter.
 */
private[scalatest] class FlexReporter(directory: String) extends Reporter {
  final val BufferSize = 4096

  private val events = ListBuffer[Event]()
  private var index = 0

  //
  // Records events as they are received.  Initiates processing once
  // a run-termination event comes in.
  //
  def apply(event: Event) {
    event match {
      case _: RunStarting  =>
      case _: RunCompleted => writeFile(event)
      case _: RunStopped   => writeFile(event)
      case _: RunAborted   => writeFile(event)
      case _ => events += event
    }
  }

  //
  // Provides sequential index values for xml entries.
  //
  def nextIndex(): Int = {
    index += 1
    index
  }

  //
  // Throws exception for specified unexpected event.
  //
  def unexpectedEvent(e: Event) {
    throw new RuntimeException("unexpected event [" + e + "]")
  }

  //
  // Escapes html entities and curly braces in specified string.
  //
  def escape(s: String): String =
    scala.xml.Utility.escape(s).
      replaceAll("""\{""", """\\{""").
      replaceAll("""\}""", """\\}""")

  //
  // Formats date for inclusion in as 'date' attribute in xml.
  //
  // E.g.: "Mon May 30 10:29:58 PDT 2011"
  //
  def formatDate(timeStamp: Long): String = {
    val df = new SimpleDateFormat("EEE MMM d kk:mm:ss zzz yyyy")
    df.format(new Date(timeStamp))
  }

  //
  // Writes output file suitedata.xml to specified directory.
  //
  def writeFile(event: Event) {
    index = 0
    var suiteRecord: SuiteRecord = null
    val stack = new Stack[SuiteRecord]
    val pw =
      new PrintWriter(
        new BufferedOutputStream(
          new FileOutputStream(
            new File(directory, "suitedata.xml")), BufferSize))


    //
    // Formats <summary> element of output xml.
    //
    def formatSummary(event: Event): String = {
      val (summaryOption, durationOption) =
        event match {
          case e: RunCompleted => (e.summary, e.duration)
          case e: RunAborted   => (e.summary, e.duration)
          case e: RunStopped   => (e.summary, e.duration)
          case _ => unexpectedEvent(event); (None, None)
        }

      val summary  = summaryOption.getOrElse(Summary(0, 0, 0, 0, 0, 0, 0))
      val duration = durationOption.getOrElse(0)

      "<summary index=\"" + nextIndex() + "\" text=\"\" " +
      "duration=\""             + duration                     + "\" " +
      "testsSucceededCount=\""  + summary.testsSucceededCount  + "\" " +
      "testsFailedCount=\""     + summary.testsFailedCount     + "\" " +
      "testsIgnoredCount=\""    + summary.testsIgnoredCount    + "\" " +
      "testsPendingCount=\""    + summary.testsPendingCount    + "\" " +
      "testsCancelledCount=\""  + summary.testsCanceledCount   + "\" " +
      "suitesCompletedCount=\"" + summary.suitesCompletedCount + "\" " +
      "suitesAbortedCount=\""   + summary.suitesAbortedCount   + "\" " +
      "date=\""                 + formatDate(event.timeStamp)  + "\" " +
      "thread=\""               + event.threadName             + "\"/>\n"
    }

    //
    // Closes out a SuiteRecord.  Gets called upon receipt of a
    // SuiteCompleted or SuiteAborted event.
    //
    // If the suite being closed is nested within another suite, its
    // completed record is added to the record of the suite it is nested
    // in.  Otherwise its xml is written to the output file.
    //
    def endSuite(e: Event) {
      suiteRecord.addEndEvent(e)

      val prevRecord = stack.pop()

      if (prevRecord != null)
        prevRecord.addNestedElement(suiteRecord)
      else
        pw.print(suiteRecord.toXml)

      suiteRecord = prevRecord
    }

    //
    // writeFile main
    //
    pw.println("<doc>")
    pw.print(formatSummary(event))

    val sortedEvents = events.toList.sortWith((a, b) => a.ordinal < b.ordinal)

    for (event <- sortedEvents) {
      event match {
        case e: SuiteStarting  =>
          stack.push(suiteRecord)
          suiteRecord = new SuiteRecord(e)
          
        case e: InfoProvided   => suiteRecord.addNestedElement(e)
        case e: ScopeOpened    => suiteRecord.addNestedElement(e)
        case e: ScopeClosed    => suiteRecord.addNestedElement(e)
        case e: MarkupProvided => suiteRecord.addNestedElement(e)
        case e: TestStarting   => suiteRecord.addNestedElement(e)
        case e: TestSucceeded  => suiteRecord.addNestedElement(e)
        case e: TestIgnored    => suiteRecord.addNestedElement(e)
        case e: TestFailed     => suiteRecord.addNestedElement(e)
        case e: TestPending    => suiteRecord.addNestedElement(e)
        case e: TestCanceled   => suiteRecord.addNestedElement(e)

        case e: SuiteCompleted => endSuite(e)
        case e: SuiteAborted   => endSuite(e)

        case e: RunStarting  => unexpectedEvent(e)
        case e: RunCompleted => unexpectedEvent(e)
        case e: RunStopped   => unexpectedEvent(e)
        case e: RunAborted   => unexpectedEvent(e)
      }
    }
    pw.println("</doc>")
    pw.flush()
    pw.close()
  }

  //
  // Generates xml for an InfoProvided event.
  //
  def formatInfoProvided(event: InfoProvided): String = {
    "<info index=\"" + nextIndex()                 + "\" " +
    "text=\""        + escape(event.message)       + "\" " +
    "date=\""        + formatDate(event.timeStamp) + "\" " +
    "thread=\""      + event.threadName            + "\"/>\n"
  }

  //
  // Generates xml for ScopeOpened event.
  //
  def formatScopeOpened(event: ScopeOpened): String = {
    "<info index=\"" + nextIndex()                 + "\" " +
    "text=\""        + escape(event.message)       + "\" " +
    "date=\""        + formatDate(event.timeStamp) + "\" " +
    "thread=\""      + event.threadName            + "\">\n"
  }

  //
  // Generates xml for a MarkupProvided event.
  //
  def formatMarkupProvided(event: MarkupProvided): String = {
    "<markup index=\"" + nextIndex()                 + "\" "   +
    "date=\""          + formatDate(event.timeStamp) + "\" "   +
    "thread=\""        + event.threadName            + "\">\n" +
    "<data><![CDATA["  + event.text                  + "]]></data>\n" +
    "</markup>\n"
  }

  //
  // Generates xml for a TestIgnored event.
  //
  def formatTestIgnored(event: TestIgnored): String = {
    "<test index=\"" + nextIndex() + "\" " +
    "result=\"ignored\" " +
    "text=\"" + testMessage(event.testName, event.formatter) + "\" " +
    "name=\"" + escape(event.testName) + "\" " +
    "date=\"" + formatDate(event.timeStamp) + "\" " +
    "thread=\"" + event.threadName + "\"" +
    ">\n"
  }

  //
  // Extracts message from specified formatter if there is one, otherwise
  // returns test name.
  //
  def testMessage(testName: String, formatter: Option[Formatter]): String = {
    val message =
      formatter match {
        case Some(IndentedText(_, rawText, _)) => rawText
        case _ => testName
      }
    escape(message)
  }

  //
  // Class that aggregates events that make up a suite.
  //
  // Holds all the events encountered from SuiteStarting through its
  // corresponding end event (e.g. SuiteCompleted).  Once the end event
  // is received, this class's toXml method can be called to generate the
  // complete xml string for the <suite> element.
  //
  class SuiteRecord(startEvent: SuiteStarting) {
    var nestedElements = List[Any]()
    var endEvent: Event = null

    //
    // Adds either an Event or a nested SuiteRecord to this object's
    // list of elements.
    //
    def addNestedElement(element: Any) {
      nestedElements ::= element
    }

    //
    // Adds suite closing event (SuiteCompleted or SuiteAborted) to the
    // object.
    //
    def addEndEvent(event: Event) {
      def isEndEvent(e: Event): Boolean = {
        e match {
          case _: SuiteCompleted => true
          case _: SuiteAborted   => true
          case _ => false
        }
      }

      require(endEvent == null)
      require(isEndEvent(event))

      endEvent = event
    }

    //
    // Generates value to be used in <suite> element's 'result' attribute.
    //
    def result: String = {
      endEvent match {
        case _: SuiteCompleted => "completed"
        case _: SuiteAborted   => "aborted"
        case _ => unexpectedEvent(endEvent); ""
      }
    }

    //
    // Generates xml string representation of object.
    //
    def toXml: String = {
      val buf = new StringBuilder
      var testRecord: TestRecord = null

      //
      // Generates opening <suite ...> element
      //
      def formatStartOfSuite: String =
        "\n" +
        "<suite index=\"" + nextIndex()                      + "\" " +
        "result=\""       + result                           + "\" " +
        "name=\""         + escape(startEvent.suiteName)     + "\" " +
        "date=\""         + formatDate(startEvent.timeStamp) + "\" " +
        "thread=\""       + startEvent.threadName            + "\">\n"

      //
      // Indicates whether a test record is currently open during
      // event processing.
      //
      def inATest: Boolean =
        (testRecord != null) && (testRecord.endEvent == null)

      //
      // toXml main
      //
      buf.append(formatStartOfSuite)

      for (element <- nestedElements.reverse) {
        if (inATest) {
          testRecord.addEvent(element.asInstanceOf[Event])

          if (testRecord.isComplete)
            buf.append(testRecord.toXml)
        }
        else {
          element match {
            case e: InfoProvided   => buf.append(formatInfoProvided(e))
            case e: ScopeOpened    => buf.append(formatScopeOpened(e))
            case e: ScopeClosed    => buf.append("</info>\n")
            case e: MarkupProvided => buf.append(formatMarkupProvided(e))
            case e: TestIgnored    => buf.append(formatTestIgnored(e))
            case e: SuiteRecord    => buf.append(e.toXml)
            case e: TestStarting   => testRecord = new TestRecord(e)
            case _ =>
              throw new RuntimeException("unexpected [" + element + "]")
          }
        }
      }
      buf.toString + "</suite>\n"
    }
  }

  //
  // Class that aggregates events that make up a test.
  //
  // Holds all the events encountered from TestStarting through its
  // corresponding end event (e.g. TestSucceeded).  Once the end event
  // is received, this class's toXml method can be called to generate
  // the complete xml string for the <test> element.
  //
  class TestRecord(startEvent: TestStarting) {
    var nestedEvents = List[Event]()
    var endEvent: Event = null

    //
    // Adds specified event to object's list of nested events.
    //
    def addEvent(event: Event) {
      def isNestedEvent: Boolean = {
        event match {
          case _: InfoProvided => true
          case _: ScopeOpened => true
          case _: ScopeClosed => true
          case _: MarkupProvided => true
          case _ => false
        }
      }

      def isEndEvent: Boolean = {
        event match {
          case _: TestSucceeded => true
          case _: TestFailed => true
          case _: TestPending => true
          case _: TestCanceled => true
          case _ => false
        }
      }

      if (isNestedEvent)
        nestedEvents ::= event
      else if (isEndEvent)
        endEvent = event
      else
        unexpectedEvent(event)
    }

    //
    // Indicates whether an end event has been received yet for this
    // record.
    //
    def isComplete: Boolean = (endEvent != null)

    //
    // Generates value for use as 'result' attribute of <test> element.
    //
    def result: String = {
      endEvent match {
        case _: TestSucceeded => "passed"
        case _: TestFailed    => "failed"
        case _: TestPending   => "pending"
        case _: TestCanceled  => "canceled"
        case _ => unexpectedEvent(endEvent); ""
      }
    }

    //
    // Generates initial <test> element of object's xml.
    //
    def formatTestStart: String = {
      "<test index=\"" + nextIndex() + "\" " +
      "result=\"" + result + "\" " +
      "text=\"" + testMessage(startEvent.testName, startEvent.formatter) +
      "\" " +
      "name=\"" + escape(startEvent.testName) + "\" " +
      "date=\"" + formatDate(startEvent.timeStamp) + "\" " +
      "thread=\"" + startEvent.threadName + "\"" +
      ">\n"
    }

    //
    // Generates <exception> xml for a test failure.
    //
    def formatException(event: TestFailed): String = {
      val buf = new StringBuilder
      var depth = -1

      def nextDepth: Int = {
        depth += 1
        depth
      }

      buf.append("<exception ")

      if (event.suiteClassName.isDefined)
        buf.append("className=\"" + event.suiteClassName.get + "\"")

      buf.append(">\n")
      
      if (event.throwable.isDefined) {
        val throwable = event.throwable.get
        val stackTrace = throwable.getStackTrace
        require(stackTrace.size > 0)

        buf.append("<message>" + event.message + "</message>\n")

        if (throwable.isInstanceOf[StackDepthException]) {
          val sde = throwable.asInstanceOf[StackDepthException]

          if (sde.failedCodeFileName.isDefined &&
              sde.failedCodeLineNumber.isDefined)
          {
            buf.append(
              "<stackDepth>\n" +
              "<depth>" + sde.failedCodeStackDepth + "</depth>\n" +
              "<fileName>" + sde.failedCodeFileName.get + "</fileName>\n" +
              "<lineNumber>" +
                sde.failedCodeLineNumber.get +
              "</lineNumber>\n" +
              "</stackDepth>\n")
          }
        }

        buf.append("<stackTrace>\n")
        for (frame <- stackTrace) {
          buf.append(
            "<stackFrame depth=\"" + nextDepth + "\">" +
              frame.getClassName + "(" + frame.getFileName + ":" +
              frame.getLineNumber + ")" +
            "</stackFrame>\n")
        }
        buf.append("</stackTrace>\n")
      }
      buf.append("</exception>\n")




      buf.toString
    }

    //
    // Generates xml string representation of object.
    //
    def toXml: String = {
      val buf = new StringBuilder

      if (endEvent == null)
        throw new IllegalStateException("toXml called without endEvent")

      buf.append(formatTestStart)

      for (event <- nestedEvents) {
        event match {
          case e: InfoProvided   => buf.append(formatInfoProvided(e))
          case e: ScopeOpened    => buf.append(formatScopeOpened(e))
          case e: ScopeClosed    => buf.append("</info>\n")
          case e: MarkupProvided => buf.append(formatMarkupProvided(e))
          case _ => unexpectedEvent(event)
        }
      }

      if (endEvent.isInstanceOf[TestFailed])
        buf.append(formatException(endEvent.asInstanceOf[TestFailed]))

      buf.append("</test>\n")

      buf.toString
    }
  }
}

