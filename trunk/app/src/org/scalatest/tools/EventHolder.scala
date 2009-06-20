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


import events.{IndentedText, Event}

/**
 * Used to hold Reports in the GUI, so that I can keep track of which report method was called
 * on the reporter to deliver it.
 *
 * @author Bill Venners
 */
private[scalatest] class EventHolder(val event: Event, val message: Option[String], val throwable: Option[Throwable],
    val rerunner: Option[Rerunner], val eventType: ReporterOpts.Value, val isRerun: Boolean) {

  if (event == null || eventType == null)
    throw new NullPointerException()
 
  def this(event: Event, message: Option[String], throwable: Option[Throwable], rerunner: Option[Rerunner],
           eventType: ReporterOpts.Value) = this(event, message, throwable, rerunner, eventType, false)

  override def toString = {
    event.formatter match {
      case Some(IndentedText(_, rawText, indentationLevel)) => rawText
      case _ => event.toString
    }
  }

/*
  override def toString(): String = {

    event match {
      case sr: SpecReport =>
        if (eventType == ReporterOpts.PresentSuiteStarting)
          sr.plainSpecText + ":"
        else 
          sr.plainSpecText
      case _ => 
        val firstString: String =
          if (isRerun)
            Resources("RERUN_" + ReporterOpts.getUpperCaseName(eventType))
          else
            Resources(ReporterOpts.getUpperCaseName(eventType))

        if (eventType == ReporterOpts.PresentRunStarting || eventType == ReporterOpts.PresentRunStopped ||
            eventType == ReporterOpts.PresentRunAborted || eventType == ReporterOpts.PresentRunCompleted) {

          firstString 
        }
        else if (eventType == ReporterOpts.PresentInfoProvided) {
          firstString + " - " + event.message
        }
        else {
          firstString + " - " + event.name
        }
    }
  }
*/
}
