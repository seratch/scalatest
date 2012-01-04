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
import org.scalatest.Suite
import org.scalatest.ParallelTestExecution

private[scalatest] class DistributedTestRunnerSuite(suite: ParallelTestExecution, testName: String) extends Suite {
  override def run(ignoreThisTestName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
          configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
    suite.runOneTest(testName, reporter, stopper, configMap, tracker)
  }
}