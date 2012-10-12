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
import java.net.URL
import scala.io.Source
import java.nio.channels.Channels

/**
 * A <code>Reporter</code> that prints test status information in HTML format to a file.
 */
private[scalatest] class HtmlReporter(directoryPath: String, presentAllDurations: Boolean,
        presentInColor: Boolean, presentStackTraces: Boolean, presentFullStackTraces: Boolean, cssUrl: URL) extends ResourcefulReporter {

  private val directory = new File(directoryPath)
  if (!directory.exists)
    directory.mkdirs()
    
  private def copyResource(url: URL, targetFileName: String) {
    val inputStream = url.openStream
    val outputStream = new FileOutputStream(new File(directory, targetFileName))
    outputStream getChannel() transferFrom(Channels.newChannel(inputStream), 0, Long.MaxValue)
    inputStream.close()
    outputStream.flush()
    outputStream.close()
  }
  
  copyResource(cssUrl, "styles.css")
  copyResource(classOf[Suite].getClassLoader.getResource("org/scalatest/sorttable.js"), "sorttable.js")
  copyResource(classOf[Suite].getClassLoader.getResource("org/scalatest/d3.v2.min.js"), "d3.v2.min.js")
    
  /*val cssInputStream = cssUrl.openStream
  val cssOutputStream = new FileOutputStream(new File(directory, "styles.css"))
  cssOutputStream getChannel() transferFrom(Channels.newChannel(cssInputStream), 0, Long.MaxValue)
  cssInputStream.close()
  cssOutputStream.flush()
  cssOutputStream.close()
  
  val sortTableInputStream = classOf[Suite].getClassLoader.getResource("org/scalatest/sorttable.js").openStream
  val sortTableOutputStream = new FileOutputStream(new File(directory, "sorttable.js"))
  sortTableOutputStream getChannel() transferFrom(Channels.newChannel(sortTableInputStream), 0, Long.MaxValue)
  sortTableInputStream.close()
  sortTableOutputStream.flush()
  sortTableOutputStream.close()*/
  
  private val pegDown = new PegDownProcessor

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
  
  private def makeIndexFile(resourceName: String, duration: Option[Long], summary: Option[Summary]) {
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(directory, "index.html")), BufferSize))
    pw.println {
      """
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html
  PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
        
      """ + getIndexHtml(resourceName, duration, summary) 
    }
    pw.flush()
  }
  
  private def getHeaderStatusColor(summary: Option[Summary]) = 
    summary match {
      case Some(summary) => if (summary.testsFailedCount == 0) "scalatest-header-passed" else "scalatest-header-failed"
      case None => "scalatest-header-failed"
    }
  
  private def getPieChartScript = {
    val (succeeded, failed, ignored, pending, canceled) = suiteList.foldLeft((0, 0, 0, 0, 0)) { case ((succeeded, failed, ignored, pending, canceled), r) =>
      (succeeded + r.testsSucceededCount, failed + r.testsFailedCount, ignored + r.testsIgnoredCount, 
       pending + r.testsPendingCount, canceled + r.testsCanceledCount)
    }
    
    """
    /* modified from http://www.permadi.com/tutorial/cssGettingBackgroundColor/index.html - */
    function getBgColor(elementId) 
    {
      var element = document.getElementById(elementId);
      if (element.currentStyle)
        return element.currentStyle.backgroundColor;
      if (window.getComputedStyle)
      {
        var elementStyle=window.getComputedStyle(element,"");
        if (elementStyle)
          return elementStyle.getPropertyValue("background-color");
      }
      // Return 0 if both methods failed.  
      return 0;
    }
    
    """ + 
    "var data = [" + succeeded + ", " + failed + ", " + ignored + ", " + pending + ", " + canceled + "];" + 
    "var color = [getBgColor('summary_view_row_1_legend_succeeded'), " + 
    "             getBgColor('summary_view_row_1_legend_failed'), " + 
    "             getBgColor('summary_view_row_1_legend_ignored'), " + 
    "             getBgColor('summary_view_row_1_legend_pending'), " +
    "             getBgColor('summary_view_row_1_legend_canceled')" + 
    "            ];" + 
    """
    var width = document.getElementById('chart_div').offsetWidth,
        height = document.getElementById('chart_div').offsetHeight,
        outerRadius = Math.min(width, height) / 2,
        innerRadius = 0,
        donut = d3.layout.pie(),
        arc = d3.svg.arc().innerRadius(innerRadius).outerRadius(outerRadius);
    
    var vis = d3.select("#chart_div")
                .append("svg")
                .data([data])
                .attr("width", width)
                .attr("height", height);
    
    var arcs = vis.selectAll("g.arc")
                  .data(donut)
                  .enter().append("g")
                  .attr("class", "arc")
                  .attr("transform", "translate(" + outerRadius + "," + outerRadius + ")");
    
    arcs.append("path")
        .attr("fill", function(d, i) { return color[i]; })
        .attr("d", arc);
    """
  }
  
  /*private def getPieChartScript(summary: Option[Summary]) = 
    summary match {
      case Some(summary) => 
        val statusData: Array[String] = 
          Array(
            if (summary.testsSucceededCount > 0) "['Succeeded', " + summary.testsSucceededCount + "]" else null, 
            if (summary.testsFailedCount > 0) "['Failed', " + summary.testsFailedCount + "]" else null, 
            if (summary.testsIgnoredCount > 0) "['Ignored', " + summary.testsIgnoredCount + "]" else null, 
            if (summary.testsPendingCount > 0) "['Pending', " + summary.testsPendingCount + "]" else null, 
            if (summary.testsCanceledCount > 0) "['Canceled', " + summary.testsCanceledCount + "]" else null
          ).filter(_ != null)
          
        val colorData: Array[String] = 
          Array(
            if (summary.testsSucceededCount > 0) "'#339933'" else null, 
            if (summary.testsFailedCount > 0) "'#993333'" else null, 
            if (summary.testsIgnoredCount > 0) "'#FF6600'" else null, 
            if (summary.testsPendingCount > 0) "'#33CCCC'" else null, 
            if (summary.testsCanceledCount > 0) "'#FFCC00'" else null
          ).filter(_ != null)
          
        """
          function drawChart() {
            showPieChart();
            var data = google.visualization.arrayToDataTable(
            [
        """ +
        statusData.mkString("['Status', 'Count'],\n", ",\n", "\n") +
        """
            ]);

            var options = {
              title: 'Test Run Results', 
              sliceVisibilityThreshold: 0, 
        """ + 
        colorData.mkString("colors: [", ", ", "]\n") +
        """
            };

            var chart = new google.visualization.PieChart(document.getElementById('chart_div'));
            chart.draw(data, options);
          }
        
          if (typeof google !== 'undefined') {
            google.load("visualization", "1", {packages:["corechart"]});
            google.setOnLoadCallback(drawChart);
          }            
        """
      case None => "hidePieChart();"
    }*/
  
  private def getIndexHtml(resourceName: String, duration: Option[Long], summary: Option[Summary]) = 
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
      <head>
        <title>ScalaTest Results</title>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <meta http-equiv="Expires" content="-1" />
        <meta http-equiv="Pragma" content="no-cache" />
        <link href="styles.css" rel="stylesheet" />
        <script type="text/javascript" src="d3.v2.min.js"></script>
        <script type="text/javascript" src="sorttable.js"></script>
        <script type="text/javascript">
          { PCDATA("""
          var tagMap = {};    
              
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
            
            var SUCCEEDED_BIT = 1;
            var FAILED_BIT = 2;
            var IGNORED_BIT = 4;
            var PENDING_BIT = 8;
            var CANCELED_BIT = 16;
              
            function applyFilter() {
              var mask = 0;
              if (document.getElementById('succeeded_checkbox').checked)
                mask |= SUCCEEDED_BIT; 
              if (document.getElementById('failed_checkbox').checked)
                mask |= FAILED_BIT;
              if (document.getElementById('ignored_checkbox').checked)
                mask |= IGNORED_BIT;
              if (document.getElementById('pending_checkbox').checked)
                mask |= PENDING_BIT;
              if (document.getElementById('canceled_checkbox').checked)
                mask |= CANCELED_BIT;

              for (var key in tagMap) {
                if (tagMap.hasOwnProperty(key)) {
                  var bitSet = tagMap[key];
                  var element = document.getElementById(key);
                  if ((bitSet & mask) != 0) 
                    element.style.display = "table-row";
                  else 
                    element.style.display = "none";
                }
              }
            }
          """) }
        </script>
      </head>
      <body>
        <div class="scalatest-report"> 
          { header(resourceName, duration, summary) }
          <table id="summary_view">
            <tr id="summary_view_row_1">
              <td id="summary_view_row_1_chart">
                <div id="chart_div"></div>
              </td>
              <td id="summary_view_row_1_legend">
                <table id="summary_view_row_1_legend_table">
                  <tr id="summary_view_row_1_legend_table_row_succeeded">
                    <td id="summary_view_row_1_legend_succeeded">Succeeded</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_failed">
                    <td id="summary_view_row_1_legend_failed">Failed</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_ignored">
                    <td id="summary_view_row_1_legend_ignored">Ignored</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_pending">
                    <td id="summary_view_row_1_legend_pending">Pending</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_canceled">
                    <td id="summary_view_row_1_legend_canceled">Canceled</td>
                  </tr>
                </table>
              </td>
            </tr>
            <tr id="summary_view_row_2">
              <td id="summary_view_row_2_results" colspan="2">{ suiteResults }</td>
            </tr>
          </table>
        </div>
        <script type="text/javascript">
          { PCDATA(getPieChartScript) }
        </script>
        <script type="text/javascript">
          { PCDATA(tagMapScript) }
        </script>
      </body>
</html>
          
  private def getStatistic(summary: Option[Summary]) = {
    summary match {
      case Some(summary) => 
        <div id="display-filters">
          <input id="succeeded_checkbox" name="succeeded_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="passed_checkbox">Succeeded: { summary.testsSucceededCount }</label>
          <input id="failed_checkbox" name="failed_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="failed_checkbox">Failed: { summary.testsFailedCount }</label>
          <input id="ignored_checkbox" name="ignored_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="ignored_checkbox">Ignored: { summary.testsIgnoredCount }</label>
          <input id="pending_checkbox" name="pending_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="pending_checkbox">Pending: { summary.testsPendingCount }</label>
          <input id="canceled_checkbox" name="canceled_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="canceled_checkbox">Canceled: { summary.testsCanceledCount }</label>
        </div>
      case None => <div id="display-filters" />
    }
  }
  
  private def header(resourceName: String, duration: Option[Long], summary: Option[Summary]) = 
    <div id="scalatest-header" class={ getHeaderStatusColor(summary) }>
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
        
  private def generateElementId = UUID.randomUUID.toString
  
  private def setBit(stack: collection.mutable.Stack[String], tagMap: collection.mutable.HashMap[String, Int], bit: Int) {
    stack.foreach { scopeElementId => 
      val currentBits = tagMap(scopeElementId)
      tagMap.put(scopeElementId, currentBits | bit)
    }
  }
  
  val tagMap = collection.mutable.HashMap[String, Int]()
        
  private def suiteResults = 
    <table class="sortable">
      <tr>
        <td>Suite</td>
        <td>Succeeded</td>
        <td>Failed</td>
        <td>Ignored</td>
        <td>Pending</td>
        <td>Canceled</td>
        <td>Total</td>
      </tr>
    {
      val sortedSuiteList = suiteList.sortWith((a, b) => a.startEvent.suiteName < b.startEvent.suiteName).toArray
      sortedSuiteList map { r =>
        val elementId = generateElementId
        // use imperative style here for performance
        var succeededCount = 0
        var failedCount = 0
        var ignoredCount = 0
        var pendingCount = 0
        var canceledCount = 0
        var bits = 0
        r.eventList.foreach { e => 
          e match {
            case testSucceeded: TestSucceeded => 
              succeededCount +=1
              bits |= SUCCEEDED_BIT
            case testFailed: TestFailed => 
              failedCount += 1
              bits |= FAILED_BIT
            case testIgnored: TestIgnored => 
              ignoredCount += 1
              bits |= IGNORED_BIT
            case testPending: TestPending => 
              pendingCount += 1
              bits |= PENDING_BIT
            case testCanceled: TestCanceled => 
              canceledCount += 1
              bits |= CANCELED_BIT
            case _ => 
          }
        }
        tagMap.put(elementId, bits)
        suiteSummary(elementId, r.startEvent.suiteName, succeededCount, failedCount, ignoredCount, pendingCount, canceledCount)
      }
    
      /*val scopeStack = new collection.mutable.Stack[String]()
      eventList.map { e => 
        e match {
          
          case SuiteStarting(ordinal, suiteName, suiteId, suiteClassName, formatter, location, rerunnable, payload, threadName, timeStamp) =>
            val stringToPrint = stringToPrintWhenNoError("suiteStarting", formatter, suiteName, None)
            stringToPrint match {
              case Some(string) => 
                val elementId = generateElementId
                tagMap.put(elementId, 0)
                scopeStack.push(elementId)
                suite(elementId, suiteName, getIndentLevel(formatter))
              case None => 
                NodeSeq.Empty
            }
            
          case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
            scopeStack.clear()
            NodeSeq.Empty
            
          case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
            scopeStack.clear()
            NodeSeq.Empty
            
          case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) => 
            val testNameInfo = nameInfo.testName
            val stringToPrint = stringToPrintWhenNoError("scopeOpened", formatter, nameInfo.suiteName, nameInfo.testName)
            stringToPrint match {
              case Some(string) => 
                val elementId = generateElementId
                tagMap.put(elementId, 0)
                scopeStack.push(elementId)
                scope(elementId, string, getIndentLevel(formatter) + 1)
              case None => 
                NodeSeq.Empty
            }
            
          case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
            scopeStack.pop
            NodeSeq.Empty
          
          case TestSucceeded(ordinal, suiteName, suiteID, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 
            
            val stringToPrint = stringToPrintWhenNoError("testSucceeded", formatter, suiteName, Some(testName), duration)

            stringToPrint match {
              case Some(string) => 
                val elementId = generateElementId
                tagMap.put(elementId, SUCCEEDED_BIT)
                setBit(scopeStack, tagMap, SUCCEEDED_BIT)
                test(elementId, List(string), getIndentLevel(formatter) + 1, "test_passed")
              case None =>
                NodeSeq.Empty
            }
            
            // TODO: Print recorded events, when merge into trunk.
            
          case TestFailed(ordinal, message, suiteName, suiteID, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

            val stringToPrint = stringsToPrintOnError("failedNote", "testFailed", message, throwable, formatter, Some(suiteName), Some(testName), duration)
            val elementId = generateElementId
            tagMap.put(elementId, FAILED_BIT)
            setBit(scopeStack, tagMap, FAILED_BIT)
            testWithDetails(elementId, List(stringToPrint), message, throwable, getIndentLevel(formatter) + 1, "test_failed")            
            
            // TODO: Print recorded events, when merge into trunk.
            
          case TestIgnored(ordinal, suiteName, suiteID, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) => 

            val stringToPrint =
              formatter match {
                case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("ignoredNote")))
                case Some(MotionToSuppress) => None
                case _ => Some(Resources("testIgnored", suiteName + ": " + testName))
              }
 
              stringToPrint match {
                case Some(string) => 
                  val elementId = generateElementId
                  tagMap.put(elementId, IGNORED_BIT)
                  setBit(scopeStack, tagMap, IGNORED_BIT)
                  test(elementId, List(string), getIndentLevel(formatter) + 1, "test_ignored")
                case None =>
                  NodeSeq.Empty
              }
              
          case TestPending(ordinal, suiteName, suiteID, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>

            val stringToPrint =
              formatter match {
                case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("pendingNote")))
                case Some(MotionToSuppress) => None
                case _ => Some(Resources("testPending", suiteName + ": " + testName))
              }

            stringToPrint match {
              case Some(string) => 
                val elementId = generateElementId
                tagMap.put(elementId, PENDING_BIT)
                setBit(scopeStack, tagMap, PENDING_BIT)
                test(elementId, List(string), getIndentLevel(formatter) + 1, "test_pending")
              case None =>
                NodeSeq.Empty
            }
            
            // TODO: Print recorded events, when merge into trunk.
            
          case TestCanceled(ordinal, message, suiteName, suiteID, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, payload, threadName, timeStamp) =>

            val stringToPrint = stringsToPrintOnError("canceledNote", "testCanceled", message, throwable, formatter, Some(suiteName), Some(testName), duration)
            val elementId = generateElementId
            tagMap.put(elementId, CANCELED_BIT)
            setBit(scopeStack, tagMap, CANCELED_BIT)
            testWithDetails(elementId, List(stringToPrint), message, throwable, getIndentLevel(formatter) + 1, "test_canceled")
            
            // TODO: Print recorded events, when merge into trunk.
            
          case InfoProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>

            val (suiteName, testName) =
              nameInfo match {
                case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
                case None => (None, None)
              }
            val infoContent = stringsToPrintOnError("infoProvidedNote", "infoProvided", message, throwable, formatter, suiteName, testName, None)
            
            val elementId = generateElementId
            if (scopeStack.size > 0) {
              val topElementId = scopeStack.top
              tagMap.put(elementId, tagMap(topElementId))
            }
            test(elementId, List(infoContent), getIndentLevel(formatter) + 1, "info")
        
          case MarkupProvided(ordinal, text, nameInfo, formatter, location, payload, threadName, timeStamp) => 

            val (suiteName, testName) =
              nameInfo match {
                case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
                case None => (None, None)
              }
        
            val elementId = generateElementId
            if (scopeStack.size > 0) {
              val topElementId = scopeStack.top
              tagMap.put(elementId, tagMap(topElementId))
            }
            markup(elementId, text, getIndentLevel(formatter) + 1, "markup")
            // TO CONTINUE: XML element must be last
            
          case _ => NodeSeq.Empty
        }
      }*/
    }
    </table>
  
  private def suiteNameStyle(succeededCount: Int, failedCount: Int, ignoredCount: Int, pendingCount: Int, canceledCount: Int) = 
    if (failedCount > 0)
      "suite_name_with_failed"
    else if (ignoredCount > 0 || pendingCount > 0 || canceledCount > 0)
      "suite_name_passed"
    else
      "suite_name_passed_all"
      
  private def countStyle(prefix: String, count: Int) = 
    if (count == 0)
      prefix + "_zero"
    else
      prefix
      
  private def totalStyle(succeededCount: Int, failedCount: Int, ignoredCount: Int, pendingCount: Int, canceledCount: Int) = 
    if (failedCount > 0)
      "total_with_failed"
    else if (ignoredCount > 0 || pendingCount > 0 || canceledCount > 0)
      "total_passed"
    else
      "total_passed_all"
    
  private def suiteSummary(elementId: String, suiteName: String, succeededCount: Int, 
                            failedCount: Int, ignoredCount: Int, pendingCount: Int, canceledCount: Int) = 
    <tr id={ elementId }>
      <td class={ suiteNameStyle(succeededCount, failedCount, ignoredCount, pendingCount, canceledCount) }>{ suiteName }</td>
      <td class={ countStyle("succeeded", succeededCount) }>{ succeededCount }</td>
      <td class={ countStyle("failed", failedCount) }>{ failedCount }</td>
      <td class={ countStyle("ignored", ignoredCount) }>{ ignoredCount }</td>
      <td class={ countStyle("pending", pendingCount) }>{ pendingCount }</td>
      <td class={ countStyle("canceled", canceledCount) }>{ canceledCount }</td>
      <td class={ totalStyle(succeededCount, failedCount, ignoredCount, pendingCount, canceledCount) }>{ succeededCount + failedCount + ignoredCount + pendingCount + canceledCount }</td>
    </tr>
        
  private def suite(elementId: String, suiteName: String, indentLevel: Int) = 
    <div id={ elementId } class="suite" style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        <dt>{ suiteName }</dt>
      </dl>
    </div>
        
  private def scope(elementId: String, message: String, indentLevel: Int) = 
    <div id={ elementId } class="scope" style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      { message }
    </div>
      
  private def test(elementId: String, lines: List[String], indentLevel: Int, styleName: String) = 
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        {
          lines.map { line => 
            <dt>{ line }</dt>
          }
        }
      </dl>
    </div>
  
  private def testWithDetails(elementId: String, lines: List[String], message: String, throwable: Option[Throwable], indentLevel: Int, styleName: String) = {
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
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
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
        
  private def markup(elementId: String, text: String, indentLevel: Int, styleName: String) = 
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
       { XML.loadString(pegDown.markdownToHtml(text)) }
    </div>
       
  private def tagMapScript = 
    "tagMap = { \n" + 
      tagMap.map { case (elementId, bitSet) => "\"" + elementId + "\": " + bitSet }.mkString(", \n") + 
    "};\n" + 
    "applyFilter();"

  case class SuiteResult(suiteId: String, suiteName: String, suiteClassName: Option[String], startEvent: SuiteStarting, endEvent: Event, eventList: IndexedSeq[Event], 
                          testsSucceededCount: Int, testsFailedCount: Int, testsIgnoredCount: Int, testsPendingCount: Int, testsCanceledCount: Int)
    
  private var eventList = new ListBuffer[Event]()
  private val suiteList = new ListBuffer[SuiteResult]()
  private var runEndEvent: Option[Event] = None
        
  def apply(event: Event) {
        
    event match {

      case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) => 

      case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 
        runEndEvent = Some(event)

      case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
        runEndEvent = Some(event)

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 
        runEndEvent = Some(event)
        
      case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
        val (suiteEvents, otherEvents) = extractSuiteEvents(suiteId)
        eventList = otherEvents
        val sortedSuiteEvents = suiteEvents.sorted
        sortedSuiteEvents.head match {
          case suiteStarting: SuiteStarting => 
            suiteList += sortedSuiteEvents.foldLeft(SuiteResult(suiteId, suiteName, suiteClassName, suiteStarting, event, sortedSuiteEvents.tail.toIndexedSeq, 0, 0, 0, 0, 0)) { case (r, e) => 
              e match {
                case testSucceeded: TestSucceeded => r.copy(testsSucceededCount = r.testsSucceededCount + 1)
                case testFailed: TestFailed => r.copy(testsFailedCount = r.testsFailedCount + 1)
                case testIgnored: TestIgnored => r.copy(testsIgnoredCount = r.testsIgnoredCount + 1)
                case testPending: TestPending => r.copy(testsPendingCount = r.testsPendingCount + 1)
                case testCanceled: TestCanceled => r.copy(testsCanceledCount = r.testsCanceledCount + 1)
                case _ => r
              }
            }
          case other => 
            throw new IllegalStateException("Expected SuiteStarting in the head of suite events, but we got: " + other.getClass.getName)
        }
            
      case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
        val (suiteEvents, otherEvents) = extractSuiteEvents(suiteId)
        eventList = otherEvents
        val sortedSuiteEvents = suiteEvents.sorted
        sortedSuiteEvents.head match {
          case suiteStarting: SuiteStarting => 
            suiteList += sortedSuiteEvents.foldLeft(SuiteResult(suiteId, suiteName, suiteClassName, suiteStarting, event, sortedSuiteEvents.tail.toIndexedSeq, 0, 0, 0, 0, 0)) { case (r, e) => 
              e match {
                case testSucceeded: TestSucceeded => r.copy(testsSucceededCount = r.testsSucceededCount + 1)
                case testFailed: TestFailed => r.copy(testsFailedCount = r.testsFailedCount + 1)
                case testIgnored: TestIgnored => r.copy(testsIgnoredCount = r.testsIgnoredCount + 1)
                case testPending: TestPending => r.copy(testsPendingCount = r.testsPendingCount + 1)
                case testCanceled: TestCanceled => r.copy(testsCanceledCount = r.testsCanceledCount + 1)
                case _ => r
              }
            }
          case other => 
            throw new IllegalStateException("Expected SuiteStarting in the head of suite events, but we got: " + other.getClass.getName)
        }
      
      case _ => eventList += event
    }
  }
      
  def extractSuiteEvents(suiteId: String) = eventList partition { e => 
    e match {
      case e: TestStarting => e.suiteId == suiteId
      case e: TestSucceeded  => e.suiteId == suiteId
      case e: TestIgnored    => e.suiteId == suiteId
      case e: TestFailed     => e.suiteId == suiteId
      case e: TestPending    => e.suiteId == suiteId
      case e: TestCanceled   => e.suiteId == suiteId
      case e: InfoProvided   => 
        e.nameInfo match {
          case Some(nameInfo) => 
            nameInfo.suiteID == suiteId
          case None => false
        }
      case e: MarkupProvided => 
        e.nameInfo match {
          case Some(nameInfo) => 
            nameInfo.suiteID == suiteId
          case None => false
        }
      case e: ScopeOpened    => e.nameInfo.suiteID == suiteId
      case e: ScopeClosed    => e.nameInfo.suiteID == suiteId
      case e: SuiteStarting  => e.suiteId == suiteId
      case _ => false
    }
  }
      
  def dispose() {
    runEndEvent match {
      case Some(event) => 
        event match {
          case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 
            makeIndexFile("runCompleted", duration, summary)

          case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
            makeIndexFile("runStopped", duration, summary)

          case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 
            makeIndexFile("runAborted", duration, summary)
            
          case other =>
            throw new IllegalStateException("Expected run ending event only, but got: " + other.getClass.getName)
        }
      case None => // If no run end event (e.g. when run in sbt), just use runCompleted
        makeIndexFile("runCompleted", None, None)
    }
    
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
  final val SUCCEEDED_BIT = 1
  final val FAILED_BIT = 2
  final val IGNORED_BIT = 4
  final val PENDING_BIT = 8
  final val CANCELED_BIT = 16
}

private[tools] object PCDATA {
  def apply(in: String): Node = scala.xml.Unparsed(in)
}