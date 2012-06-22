/*
 * Copyright 2001-2008 Artima, Inc.
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

import org.scalatest._
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.util.Iterator
import java.util.Set
import java.io.StringWriter
import org.scalatest.events._
import org.scalatest.exceptions.TestFailedException
import PrintReporter.{BufferSize, makeDurationString}
import HtmlReporter._
import org.pegdown.PegDownProcessor
import scala.collection.mutable.ListBuffer
import scala.xml.NodeSeq
import scala.xml.XML

/**
 * A <code>Reporter</code> that prints test status information in HTML format to a file.
 */
private[scalatest] class HtmlReporter(pw: PrintWriter, presentAllDurations: Boolean,
        presentInColor: Boolean, presentStackTraces: Boolean, presentFullStackTraces: Boolean) extends ResourcefulReporter {

  private val pegDown = new PegDownProcessor
  
  /**
  * Construct a <code>PrintReporter</code> with passed
  * <code>String</code> file name. Information about events reported to instances of this
  * class will be written to the specified file using the
  * default character encoding.
  *
  * @param filename the <code>String</code> name of the file to which to print reported info
  * @throws NullPointerException if passed <code>filename</code> reference is <code>null</code>
  * @throws IOException if unable to open the specified file for writing
  */
  def this(
    filename: String,
    presentAllDurations: Boolean,
    presentInColor: Boolean,
    presentShortStackTraces: Boolean,
    presentFullStackTraces: Boolean
  ) =
    this(
      new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(filename)), BufferSize)),
      presentAllDurations,
      presentInColor,
      presentShortStackTraces,
      presentFullStackTraces
    )

  private def withPossibleLineNumber(stringToPrint: String, throwable: Option[Throwable]): String = {
    throwable match {
      case Some(testFailedException: TestFailedException) =>
        testFailedException.failedCodeFileNameAndLineNumberString match {
          case Some(lineNumberString) =>
            Resources("printedReportPlusLineNumber", stringToPrint, lineNumberString)
          case None => stringToPrint
        }
      case _ => stringToPrint
    }
  }

  // Called for TestFailed, InfoProvided (because it can have a throwable in it), and SuiteAborted
  private def stringsToPrintOnError(noteResourceName: String, errorResourceName: String, message: String, throwable: Option[Throwable],
    formatter: Option[Formatter], suiteName: Option[String], testName: Option[String], duration: Option[Long]): List[String] = {

    val stringToPrint =
      formatter match {
        case Some(IndentedText(formattedText, _, _)) =>
          Resources("specTextAndNote", formattedText, Resources(noteResourceName))
        case _ =>
          // Deny MotionToSuppress directives in error events, because error info needs to be seen by users
            suiteName match {
              case Some(sn) =>
                testName match {
                  case Some(tn) => Resources(errorResourceName, sn + ": " + tn)
                  case None => Resources(errorResourceName, sn)
                }
              // Should not get here with built-in ScalaTest stuff, but custom stuff could get here.
              case None => Resources(errorResourceName, Resources("noNameSpecified"))
            }
    }

    val stringToPrintWithPossibleLineNumber = withPossibleLineNumber(stringToPrint, throwable)

    val stringToPrintWithPossibleLineNumberAndDuration =
      duration match {
        case Some(milliseconds) =>
          if (presentAllDurations)
            Resources("withDuration", stringToPrintWithPossibleLineNumber, makeDurationString(milliseconds))
          else
            stringToPrintWithPossibleLineNumber
        case None => stringToPrintWithPossibleLineNumber
      }

    // If there's a message, put it on the next line, indented two spaces, unless this is an IndentedText
    val possiblyEmptyMessage =
      formatter match {
        case Some(IndentedText(_, _, _)) => ""
        case _ =>
          Reporter.messageOrThrowablesDetailMessage(message, throwable)
      }

    // I don't want to put a second line out there if the event's message contains the throwable's message,
    // or if niether the event message or throwable message has any message in it.
    val throwableIsATestFailedExceptionWithRedundantMessage =
      throwable match {
        case Some(t) =>
          t.isInstanceOf[TestFailedException] && ((t.getMessage != null &&
          !t.getMessage.trim.isEmpty && possiblyEmptyMessage.indexOf(t.getMessage.trim) != -1) || // This part is where a throwable message exists
          (possiblyEmptyMessage.isEmpty && (t.getMessage == null || t.getMessage.trim.isEmpty))) // This part detects when both have no message
        case None => false
      }

    def getStackTrace(throwable: Option[Throwable]): List[String] =
      throwable match {
        case Some(throwable) =>

          def useConciseTestFailedExceptionForm =
            !presentFullStackTraces && (
              throwable match {
                case tfe: TestFailedException => tfe.cause.isEmpty // If there's a cause inside, show the whole stack trace
                case _ => false
              }
            )

          def stackTrace(throwable: Throwable, isCause: Boolean): List[String] = {
            val className = throwable.getClass.getName 
            val labeledClassName = if (isCause) Resources("DetailsCause") + ": " + className else className
            val labeledClassNameWithMessage =
              if (throwable.getMessage != null && !throwable.getMessage.trim.isEmpty)
                if (!useConciseTestFailedExceptionForm)
                  "  " + labeledClassName + ": " + throwable.getMessage.trim
                else
                  "  " + throwable.getMessage.trim // Don't show "org.scalatest.TestFailedException: " if no stack trace to follow
              else
                "  " + labeledClassName

            if (!useConciseTestFailedExceptionForm) {
              val stackTraceElements = throwable.getStackTrace.toList map { "  " + _.toString } // Indent each stack trace item two spaces
              val cause = throwable.getCause

              val stackTraceThisThrowable = labeledClassNameWithMessage :: stackTraceElements
              if (cause == null)
                stackTraceThisThrowable
              else
                stackTraceThisThrowable ::: stackTrace(cause, true) // Not tail recursive, but shouldn't be too deep
            }
            else List(labeledClassNameWithMessage)
          }
          if (!throwableIsATestFailedExceptionWithRedundantMessage || !useConciseTestFailedExceptionForm)
            stackTrace(throwable, false)
          else List()
        case None => List()
      }

    if (possiblyEmptyMessage.isEmpty)
      stringToPrintWithPossibleLineNumberAndDuration :: getStackTrace(throwable)
    else
      stringToPrintWithPossibleLineNumberAndDuration :: "  " + possiblyEmptyMessage :: getStackTrace(throwable)
  }

  private def stringToPrintWhenNoError(resourceName: String, formatter: Option[Formatter], suiteName: String, testName: Option[String]): Option[String] =
    stringToPrintWhenNoError(resourceName, formatter, suiteName, testName, None)

  private def stringToPrintWhenNoError(resourceName: String, formatter: Option[Formatter], suiteName: String, testName: Option[String], duration: Option[Long]): Option[String] = {

    formatter match {
      case Some(IndentedText(formattedText, _, _)) =>
        duration match {
          case Some(milliseconds) =>
            if (presentAllDurations)
              Some(Resources("withDuration", formattedText, makeDurationString(milliseconds)))
            else
              Some(formattedText)
          case None => Some(formattedText)
        }
      case Some(MotionToSuppress) => None
      case _ =>
        val arg =
          testName match {
            case Some(tn) => suiteName + ": " + tn
            case None => suiteName
          }
        val unformattedText = Resources(resourceName, arg)
        duration match {
          case Some(milliseconds) =>
            if (presentAllDurations)
              Some(Resources("withDuration", unformattedText, makeDurationString(milliseconds)))
            else
              Some(unformattedText)
          case None => Some(unformattedText)
        }

    }
  }
  
  private def getIndentLevel(formatter: Option[Formatter]) = 
    formatter match {
      case Some(IndentedText(formattedText, rawText, indentationLevel)) => indentationLevel
      case _ => 0
  }
  
  private def makeHtmlHeader() {
    pw.println("<html>")
    pw.println("<head><title>ScalaTest Run Result</title></head>")
    pw.println("<body bgcolor=\"black\">")
  }
  
  private def makeHtmlFile(resourceName: String, duration: Option[Long], summary: Option[Summary]) {
    pw.println {
      """
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html
  PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
        
      """ + getHtml(resourceName, duration, summary) 
    }
  }
  
  private def getSolidStatusColor(succeeded: Boolean) = 
    if (succeeded)
      green
    else
      red
      
  private def getHeaderStatusColor(summary: Option[Summary]) = 
    summary match {
      case Some(summary) => getSolidStatusColor(summary.testsFailedCount == 0)
      case None => "C20000"
    }
  
  private def getHtml(resourceName: String, duration: Option[Long], summary: Option[Summary]) = 
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
      <head>
        <title>ScalaTest Results</title>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <meta http-equiv="Expires" content="-1" />
        <meta http-equiv="Pragma" content="no-cache" />
        <style type="text/css"> { """
          body {
            margin: 0;
            padding: 0;
            background: #fff;
            font-size: 80%;
          }
    
          #rspec-header {
            background: """ + getHeaderStatusColor(summary) + """; color: #fff; height: 4.5em;
          }

          .rspec-report h1 {
            margin: 0px 10px 0px 10px;
            padding: 10px;
            font-family: "Lucida Grande", Helvetica, sans-serif;
            font-size: 1.8em;
            position: absolute;
          }

          #label {
            float:left;
          }

          #display-filters {
            float:left;
            padding: 35px 0 0 15px;
            font-family: "Lucida Grande", Helvetica, sans-serif;
          }

          #summary {
            float:right;
            padding: 5px 10px;
            font-family: "Lucida Grande", Helvetica, sans-serif;
            text-align: right;
          }

          #summary p {
            margin: 0 0 0 2px;
          }
            
          .scope {
            margin: 0 10px 5px;
            background: """ + green + """;
            color: #fff;
            font-weight: bold;
            font: normal 11px "Lucida Grande", Helvetica, sans-serif;
          }

          .test_passed {
            margin: 0 10px 5px;
            border-left: 5px solid #65C400;
            border-bottom: 1px solid #65C400;
            background: #DBFFB4; color: #3D7700;
          }

          .test_yellow {
            margin: 0 10px 5px;
            border-left: 5px solid #FAF834;
            border-bottom: 1px solid #FAF834;
            background: #FCFB98; color: #131313;
          }

          .test_failed {
            margin: 0 10px 5px;
            border-left: 5px solid #C20000;
            border-bottom: 1px solid #C20000;
            color: #C20000; background: #FFFBD3;
          }

          """ }
        </style>
        <script type="text/javascript">
      
        </script>
      </head>
      <body>
        <div class="rspec-report"> 
          { header(resourceName, duration, summary) } 
          { results(eventList.toList) } 
        </div>
      </body>
</html>
          
  private def getStatistic(summary: Option[Summary]) = {
    summary match {
      case Some(summary) => 
        <div id="display-filters">
          <input id="succeeded_checkbox" name="succeeded_checkbox" type="checkbox" /> <label for="passed_checkbox">Succeeded: { summary.testsSucceededCount }</label>
          <input id="failed_checkbox" name="failed_checkbox" type="checkbox" /> <label for="failed_checkbox">Failed: { summary.testsFailedCount }</label>
          <input id="ignored_checkbox" name="ignored_checkbox" type="checkbox" /> <label for="ignored_checkbox">Ignored: { summary.testsIgnoredCount }</label>
          <input id="pending_checkbox" name="pending_checkbox" type="checkbox" /> <label for="pending_checkbox">Pending: { summary.testsPendingCount }</label>
          <input id="canceled_checkbox" name="canceled_checkbox" type="checkbox" /> <label for="canceled_checkbox">Canceled: { summary.testsCanceledCount }</label>
        </div>
      case None => <div id="display-filters" />
    }
  }
  
  private def header(resourceName: String, duration: Option[Long], summary: Option[Summary]) = 
    <div id="rspec-header">
      <div id="label">
        <h1>ScalaTest Results</h1>
      </div>

      { getStatistic(summary) }

      <div id="summary">
        <p id="duration">{ getDuration(resourceName, duration) }</p>    
        <p id="totalTests">{ getTotalTests(summary) }</p>
        <p id="suiteSummary">{ getSuiteSummary(summary) }</p>
      </div>
    </div>
        
  private def results(eventList: List[Event]) = 
    <div class="results"> {
      eventList.map { e => 
        e match {
          
          case SuiteStarting(ordinal, suiteName, suiteId, suiteClassName, decodedSuiteName, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            val stringToPrint = stringToPrintWhenNoError("suiteStarting", formatter, suiteName, None)
            stringToPrint match {
              case Some(string) => suite(suiteId, suiteName, getIndentLevel(formatter))
              case None => NodeSeq.Empty
            }
            
          case ScopeOpened(ordinal, message, nameInfo, aboutAPendingTest, aboutACanceledTest, formatter, location, payload, threadName, timeStamp) => 
            val testNameInfo = nameInfo.testName
            val stringToPrint = stringToPrintWhenNoError("scopeOpened", formatter, nameInfo.suiteName, 
                                                         testNameInfo match {
                                                           case Some(tnInfo) => Some(tnInfo.testName)
                                                           case None => None
                                                         })
            stringToPrint match {
              case Some(string) => scope(string, getIndentLevel(formatter) + 1)
              case None => NodeSeq.Empty
            }
          
          case TestSucceeded(ordinal, suiteName, suiteID, suiteClassName, decodedSuiteName, testName, testText, decodedTestName, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

            val stringToPrint = stringToPrintWhenNoError("testSucceeded", formatter, suiteName, Some(testName), duration)

            stringToPrint match {
              case Some(string) => test(List(string), getIndentLevel(formatter) + 1, "test_passed")
              case None =>
            }
            
          case TestFailed(ordinal, message, suiteName, suiteID, suiteClassName, decodedSuiteName, testName, testText, decodedTestName, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

            val lines = stringsToPrintOnError("failedNote", "testFailed", message, throwable, formatter, Some(suiteName), Some(testName), duration)
            test(lines, getIndentLevel(formatter) + 1, "test_failed")
            
          case TestIgnored(ordinal, suiteName, suiteID, suiteClassName, decodedSuiteName, testName, testText, decodedTestName, formatter, location, payload, threadName, timeStamp) => 

            val stringToPrint =
              formatter match {
                case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("ignoredNote")))
                case Some(MotionToSuppress) => None
                case _ => Some(Resources("testIgnored", suiteName + ": " + testName))
              }
 
              stringToPrint match {
                case Some(string) => test(List(string), getIndentLevel(formatter) + 1, "test_yellow")
                case None =>
              }
              
          case TestPending(ordinal, suiteName, suiteID, suiteClassName, decodedSuiteName, testName, testText, decodedTestName, duration, formatter, location, payload, threadName, timeStamp) =>

            val stringToPrint =
              formatter match {
                case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("pendingNote")))
                case Some(MotionToSuppress) => None
                case _ => Some(Resources("testPending", suiteName + ": " + testName))
              }

            stringToPrint match {
              case Some(string) => test(List(string), getIndentLevel(formatter) + 1, "test_yellow")
              case None =>
            }
            
          case TestCanceled(ordinal, message, suiteName, suiteID, suiteClassName, decodedSuiteName, testName, testText, decodedTestName, throwable, duration, formatter, location, payload, threadName, timeStamp) =>

            val lines = stringsToPrintOnError("canceledNote", "testCanceled", message, throwable, formatter, Some(suiteName), Some(testName), duration)
            test(lines, getIndentLevel(formatter) + 1, "test_yellow")
            
          case InfoProvided(ordinal, message, nameInfo, aboutAPendingTest, aboutACanceledTest, throwable, formatter, location, payload, threadName, timeStamp) =>

            val (suiteName, testName) =
              nameInfo match {
                case Some(NameInfo(suiteName, _, _, _, testNameInfo)) => (Some(suiteName), if (testNameInfo.isDefined) Some(testNameInfo.get.testName) else None)
                case None => (None, None)
              }
            val lines = stringsToPrintOnError("infoProvidedNote", "infoProvided", message, throwable, formatter, suiteName, testName, None)
            val shouldBeYellow =
              aboutAPendingTest match {
                case Some(isPending) => isPending
                case None => false
              }
            
            test(lines, getIndentLevel(formatter) + 1, if (shouldBeYellow) "test_yellow" else "test_passed")
        
          case MarkupProvided(ordinal, text, nameInfo, aboutAPendingTest, aboutACanceledTest, formatter, location, payload, threadName, timeStamp) => 

            val (suiteName, testName) =
              nameInfo match {
                case Some(NameInfo(suiteName, _, _, _, testNameInfo)) => (Some(suiteName), if (testNameInfo.isDefined) Some(testNameInfo.get.testName) else None)
                case None => (None, None)
              }
            
            val shouldBeYellow =
              aboutAPendingTest match {
                case Some(isPending) => isPending
                case None => false
              }
        
            markup(text, getIndentLevel(formatter) + 1, if (shouldBeYellow) "test_yellow" else "test_passed")
            
          case _ => NodeSeq.Empty
        }
      }
    }
    </div>
        
  private def suite(suiteId: String, suiteName: String, indentLevel: Int) = 
    <div id={ "suite_" + HtmlEscape.escape(suiteId) } class="scope" style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        <dt>{ HtmlEscape.escape(suiteName) }</dt>
      </dl>
    </div>
        
  private def scope(message: String, indentLevel: Int) = 
    <div class="scope" style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      { message }
    </div>
      
  private def test(lines: List[String], indentLevel: Int, styleName: String) = 
    <div class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        {
          lines.map { line => 
            <dt>{ line }</dt>
          }
        }
      </dl>
    </div>
        
  private def markup(text: String, indentLevel: Int, styleName: String) = 
    <div class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
       { XML.loadString(pegDown.markdownToHtml(text)) }
    </div>

  private val eventList = new ListBuffer[Event]()
        
  def apply(event: Event) {
    
    event match {

      case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) => 

      case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 

        makeHtmlFile("runCompleted", duration, summary)

      case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>

        makeHtmlFile("runStopped", duration, summary)

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 

        makeHtmlFile("runAborted", duration, summary)
        
      case _ => eventList += event
    }

    pw.flush()
  }

  // Closes the print writer. Subclasses StandardOutReporter and StandardErrReporter override dispose to do nothing
  // so that those aren't closed.
  override def dispose() {
    pw.flush()
    pw.close()
  }
  
  private def getDuration(resourceName: String, duration: Option[Long]) = {
    duration match {
      case Some(msSinceEpoch) =>
        Resources(resourceName + "In", makeDurationString(msSinceEpoch))
      case None =>
        Resources(resourceName)
    }
  }
  
  private def getTotalTests(summary: Option[Summary]) = 
    summary match {
      case Some(summary) =>
        Resources("totalNumberOfTestsRun", summary.testsCompletedCount.toString)
      case None => ""
    }
    
    
  private def getSuiteSummary(summary: Option[Summary]) = 
    summary match {
      case Some(summary) => 
        Resources("suiteSummary", summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString)
      case None => ""
    }

  // We subtract one from test reports because we add "- " in front, so if one is actually zero, it will come here as -1
  // private def indent(s: String, times: Int) = if (times <= 0) s else ("  " * times) + s

  // Stupid properties file won't let me put spaces at the beginning of a property
  // "  {0}" comes out as "{0}", so I can't do indenting in a localizable way. For now
  // just indent two space to the left.  //  if (times <= 0) s 
  //  else Resources("indentOnce", indent(s, times - 1)) 
}

private[tools] object HtmlReporter {
  
  final val htmlGreen = "green"
  final val htmlCyan = "cyan"
  final val htmlYellow = "yellow"
  final val htmlRed = "red"
  
    
  final val green = "#65C400"
  final val red = "#C20000"
  final val yellow = "yellow"
}

// This class is partly taken from http://www.htmlescape.net/htmlescape_for_java.html, reimplemented in scala.
private[tools] object HtmlEscape {
     
  def escape(original: String): String = {
    if (original == null) 
      ""
    else {
      val chars = original.toCharArray()
      val escapedChars = chars.map { c => 
        c match {
          case 60 => "&lt;"
          case 62 => "&gt;"
          case 34 => "&quot;"
          case 38 => "&amp;"
          case '\n' => "<br/>"
          case '\r' => ""
          case ' ' => "&nbsp;"
          case _ => c.toString
        }
      }
      escapedChars.mkString
    }
  }
}