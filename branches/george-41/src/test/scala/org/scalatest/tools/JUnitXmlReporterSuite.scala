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
import org.scalatest.events.Ordinal
import org.scalatest.events.SuiteStarting
import org.scalatest.events.SuiteAborted
import org.scalatest.events.SuiteCompleted
import org.scalatest.events.TestStarting
import org.scalatest.events.TestSucceeded
import org.scalatest.events.TestIgnored
import org.scalatest.events.RecordableEvent

import java.io.File

class JUnitXmlReporterSuite extends FunSuite {

  val ord1 = new Ordinal(123)
  val ord1a = ord1.next
  val ord1b = ord1a.next
  val ord1c = ord1b.next
  val ord1d = ord1c.next

  val ord2 = new Ordinal(223)
  val ord2a = ord2.next
  val ord2b = ord2a.next
  val ord2c = ord2b.next
  val ord2d = ord2c.next
  val ord2e = ord2d.next

  val start1 =
    SuiteStarting(
      ord1a,
      "suite1",
      "suite id",
      None,
      None,
      None,
      None,
      None,
      "thread1",
      123123)

  val start2 =
    SuiteStarting(
      ord1b,
      "suite2",
      "suite id",
      None,
      None,
      None,
      None,
      None,
      "thread2",
      123223)

  val abort2 =
    SuiteAborted(
      ord1c,
      "aborted message",
      "suite2",
      "suite id",
      None,
      None,
      None,
      None,
      None,
      None)

  val complete1 =
    SuiteCompleted(
      ord1d,
      "suite1",
      "suite id",
      None,
      None,
      None,
      None,
      None,
      None,
      "thread1",
      123456)
      
  val start3 =
    SuiteStarting(
      ord2a,
      "suite3",
      "suite id 3",
      None,
      None,
      None,
      None,
      None,
      "thread1",
      123123)

  val complete3 =
    SuiteCompleted(
      ord2e,
      "suite3",
      "suite id 3",
      None,
      None,
      None,
      None,
      None,
      None,
      "thread1",
      123456)
      
  val startTest1 =
    TestStarting(
      ordinal = ord2b,
      suiteName = "suite3",
      suiteId = "suite id 3",
      suiteClassName = Some("Suite3Class"),
      testName = "a pass test",
      testText = "a pass test text")
      
  val endTest1 =
    TestSucceeded (
      ordinal = ord2c,
      suiteName = "suite3",
      suiteId = "suite id 3",
      suiteClassName = Some("Suite3Class"),
      testName = "a pass test",
      testText = "a pass test text",
      recordedEvents = Array[RecordableEvent]())
      
  val ignoreTest1 =
    TestIgnored (
      ordinal = ord2d,
      suiteName = "suite3",
      suiteId = "suite id 3",
      suiteClassName = Some("Suite3Class"),
      testName = "an ignored test",
      testText = "an ignored test text")

  val reporter = new JUnitXmlReporter("target")

  test("SuiteAborted and SuiteCompleted are recognized as test terminators") {
    reporter(start1)
    reporter(start2)
    reporter(abort2)
    reporter(complete1)

    val file1 = new File("target/TEST-suite1.xml")
    val file2 = new File("target/TEST-suite2.xml")

    assert(file1.exists)
    assert(file2.exists)

    file1.delete
    file2.delete
  }

  test("test case gets reported") {
    reporter(start3)
    reporter(startTest1)
    reporter(endTest1)
    reporter(ignoreTest1)
    reporter(complete3)

    val loadnode = xml.XML.loadFile("target/TEST-suite3.xml")
    assert(!(loadnode \\ "skipped").isEmpty)
  }
}
