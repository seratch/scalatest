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
import java.util.UUID
import scala.xml.Node
import scala.annotation.tailrec

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
  
  private def stringsToPrintOnError(noteResourceName: String, errorResourceName: String, message: String, throwable: Option[Throwable],
    formatter: Option[Formatter], suiteName: Option[String], testName: Option[String], duration: Option[Long]): String = {

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
        <style type="text/css"> { PCDATA("""
          body {
            margin: 0;
            padding: 0;
            background: #fff;
            font-size: 80%;
          }
    
          #scalatest-header {
            background: """ + getHeaderStatusColor(summary) + """; 
            color: #fff; 
            height: 4.5em;
          }
            
          #title {
            font-size: 2.0em;
            float: left;
            font-family: "Lucida Grande", Helvetica, sans-serif;
          }

          .scalatest-report h1 {
            margin: 0px 10px 0px 10px;
            padding: 10px;
            font-family: "Lucida Grande", Helvetica, sans-serif;
            font-size: 1.8em;
            position: absolute;
          }

          #display-filters {
            float:left;
            padding: 0px 0 0 0px;
            font-family: "Lucida Grande", Helvetica, sans-serif;
            font-size: 0.5em;
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
            
          .gray { 
            color: grey; 
          }
          
          .dark { 
            font-weight: bold; 
          }
            
          .label { 
            color: #444444; 
            font-weight: bold; 
          }

          """) }
        </style>
        <script type="text/javascript">
          { PCDATA("""
          function toggleDetails(contentId, linkId) {
              var ele = document.getElementById(contentId);
              var text = document.getElementById(linkId);
              if(ele.style.display == "block") {
                ele.style.display = "none";
                text.innerHTML = "Show Details";
              }
              else {
                ele.style.display = "block";
                text.innerHTML = "Hide Details";
              }
            }
          """) }
        </script>
      </head>
      <body>
        <div class="scalatest-report"> 
          { header(resourceName, duration, summary) } 
          { results(eventList.sorted.toList) } 
        </div>
      </body>
</html>
          
  private def getStatistic(summary: Option[Summary]) = {
    summary match {
      case Some(summary) => 
        <div id="display-filters">
          <input id="succeeded_checkbox" name="succeeded_checkbox" type="checkbox" checked="checked" /> <label for="passed_checkbox">Succeeded: { summary.testsSucceededCount }</label>
          <input id="failed_checkbox" name="failed_checkbox" type="checkbox" checked="checked" /> <label for="failed_checkbox">Failed: { summary.testsFailedCount }</label>
          <input id="ignored_checkbox" name="ignored_checkbox" type="checkbox" checked="checked" /> <label for="ignored_checkbox">Ignored: { summary.testsIgnoredCount }</label>
          <input id="pending_checkbox" name="pending_checkbox" type="checkbox" checked="checked" /> <label for="pending_checkbox">Pending: { summary.testsPendingCount }</label>
          <input id="canceled_checkbox" name="canceled_checkbox" type="checkbox" checked="checked" /> <label for="canceled_checkbox">Canceled: { summary.testsCanceledCount }</label>
        </div>
      case None => <div id="display-filters" />
    }
  }
  
  private def header(resourceName: String, duration: Option[Long], summary: Option[Summary]) = 
    <div id="scalatest-header">
      <div id="title">
        ScalaTest Results
        { getStatistic(summary) }
      </div>

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
              case Some(string) => suite(suiteId, decodedSuiteName.getOrElse(suiteName), getIndentLevel(formatter))
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
            
            val stringToPrint = stringToPrintWhenNoError("testSucceeded", formatter, suiteName, Some(decodedTestName.getOrElse(testName)), duration)

            stringToPrint match {
              case Some(string) => test(List(string), getIndentLevel(formatter) + 1, "test_passed")
              case None =>
            }
            
            // TODO: Print recorded events, when merge into trunk.
            
          case TestFailed(ordinal, message, suiteName, suiteID, suiteClassName, decodedSuiteName, testName, testText, decodedTestName, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

            val stringToPrint = stringsToPrintOnError("failedNote", "testFailed", message, throwable, formatter, Some(suiteName), Some(decodedTestName.getOrElse(testName)), duration)
            testWithDetails(List(stringToPrint), message, throwable, getIndentLevel(formatter) + 1, "test_failed")            
            
            // TODO: Print recorded events, when merge into trunk.
            
          case TestIgnored(ordinal, suiteName, suiteID, suiteClassName, decodedSuiteName, testName, testText, decodedTestName, formatter, location, payload, threadName, timeStamp) => 

            val stringToPrint =
              formatter match {
                case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("ignoredNote")))
                case Some(MotionToSuppress) => None
                case _ => Some(Resources("testIgnored", decodedSuiteName.getOrElse(suiteName) + ": " + decodedTestName.getOrElse(testName)))
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
                case _ => Some(Resources("testPending", decodedSuiteName.getOrElse(suiteName) + ": " + decodedTestName.getOrElse(testName)))
              }

            stringToPrint match {
              case Some(string) => test(List(string), getIndentLevel(formatter) + 1, "test_yellow")
              case None =>
            }
            
            // TODO: Print recorded events, when merge into trunk.
            
          case TestCanceled(ordinal, message, suiteName, suiteID, suiteClassName, decodedSuiteName, testName, testText, decodedTestName, throwable, duration, formatter, location, payload, threadName, timeStamp) =>

            val stringToPrint = stringsToPrintOnError("canceledNote", "testCanceled", message, throwable, formatter, Some(suiteName), Some(decodedTestName.getOrElse(testName)), duration)
            testWithDetails(List(stringToPrint), message, throwable, getIndentLevel(formatter) + 1, "test_yellow")
            // TODO: Print recorded events, when merge into trunk.
            
          case InfoProvided(ordinal, message, nameInfo, aboutAPendingTest, aboutACanceledTest, throwable, formatter, location, payload, threadName, timeStamp) =>

            val (suiteName, testName) =
              nameInfo match {
                case Some(NameInfo(suiteName, _, _, _, testNameInfo)) => (Some(suiteName), if (testNameInfo.isDefined) Some(testNameInfo.get.testName) else None)
                case None => (None, None)
              }
            val infoContent = stringsToPrintOnError("infoProvidedNote", "infoProvided", message, throwable, formatter, suiteName, testName, None)
            val shouldBeYellow =
              aboutAPendingTest match {
                case Some(isPending) => isPending
                case None => false
              }
            
            test(List(infoContent), getIndentLevel(formatter) + 1, if (shouldBeYellow) "test_yellow" else "test_passed")
        
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
    <div id={ "suite_" + suiteId } class="scope" style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        <dt>{ suiteName }</dt>
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
  
  private def testWithDetails(lines: List[String], message: String, throwable: Option[Throwable], indentLevel: Int, styleName: String) = {
    def getHTMLForStackTrace(stackTraceList: List[StackTraceElement]) =
              stackTraceList.map((ste: StackTraceElement) => <span>{ ste.toString }</span><br />)
    
    def getHTMLForCause(throwable: Throwable): scala.xml.NodeBuffer = {
      val cause = throwable.getCause
      if (cause != null) {
        <table>
          <tr valign="top">
            <td align="right"><span class="label">{ Resources("DetailsCause") + ":" }</span></td>
            <td align="left">{ cause.getClass.getName }</td>
          </tr>
          <tr valign="top">
            <td align="right"><span class="label">{ Resources("DetailsMessage") + ":" }</span></td>
            <td align="left"><span>{ if (cause.getMessage != null) cause.getMessage else Resources("None") }</span></td>
          </tr>
        </table>
        <table>
          <tr valign="top">
            <td align="left" colspan="2">{ getHTMLForStackTrace(cause.getStackTrace.toList) }</td>
          </tr>
        </table> &+ getHTMLForCause(cause)
      }
      else new scala.xml.NodeBuffer
    }
    
    val (grayStackTraceElements, blackStackTraceElements) =
      throwable match {
        case Some(throwable) =>
          val stackTraceElements = throwable.getStackTrace.toList
          throwable match {
            case tfe: TestFailedException =>
              (stackTraceElements.take(tfe.failedCodeStackDepth), stackTraceElements.drop(tfe.failedCodeStackDepth))
            case _ => (List(), stackTraceElements)
          } 
        case None => (List(), List())
      }
    
    val throwableTitle = 
      throwable match {
        case Some(throwable) => Some(throwable.getClass.getName)
        case None => None
      }
    
    val fileAndLineOption: Option[String] = 
      throwable match {
        case Some(throwable) =>
          throwable match {
            case stackDepth: StackDepth =>
              stackDepth.failedCodeFileNameAndLineNumberString
            case _ => None
          }
        case None => None
      }
    
    val linkId = UUID.randomUUID.toString
    val contentId = UUID.randomUUID.toString
    <div class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        {
          lines.map { line => 
            <dt>{ line }</dt>
          }
        }
      </dl>
      <a id={ linkId } href={ "javascript:toggleDetails('" + contentId + "', '" + linkId + "');" }>Show Details</a>
      <div id={ contentId } style="display: none">
        <table>
          {
            //if (mainMessage.isDefined) {
              <tr valign="top"><td align="left"><span class="label">{ Resources("DetailsMessage") + ":" }</span></td><td align="left">
                <span class="dark">
                { 
                  // Put <br>'s in for line returns at least, so property check failure messages look better
                  val messageLines = message.split("\n")
                  if (messageLines.size > 1)
                    messageLines.map(line => <span>{ line }<br/></span>)
                  else
                    <span>{ message }</span>
                }
                </span>
              </td></tr>
            //}
            //else <!-- -->
          }
          {
            fileAndLineOption match {
              case Some(fileAndLine) =>
                <tr valign="top"><td align="left"><span class="label">{ Resources("LineNumber") + ":" }</span></td><td align="left"><span class="dark">{ "(" + fileAndLine + ")" }</span></td></tr>
              case None =>
            }
          }
          {
            throwableTitle match {
              case Some(title) =>
                <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsThrowable") + ":" }</span></td><td align="left">{ title }</td></tr>
              case None => new scala.xml.NodeBuffer
            }
          }
        </table>
        <table>
          <tr valign="top"><td align="left" colspan="2">
            { grayStackTraceElements.map((ste: StackTraceElement) => <span class="gray">{ ste.toString }</span><br />) }
            { blackStackTraceElements.map((ste: StackTraceElement) => <span class="dark">{ ste.toString }</span><br />) }
            </td>
          </tr>
        </table>
        {
          throwable match {
            case Some(t) => getHTMLForCause(t)
            case None =>
          }
        }
      </div>
    </div>
  }
        
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

private[tools] object PCDATA {
  def apply(in: String): Node = scala.xml.Unparsed(in)
}