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
package org.scalatest

/**
 * Trait that causes that the nested suites of any suite it is mixed into to be run sequentially even if
 * a <code>Distributor</code> is passed to <code>runNestedSuites</code>. This trait overrides the 
 * <code>runNestedSuites</code> method and fowards every parameter passed to it to a superclass invocation
 * of <code>runNestedSuites</code>, except it always passes <code>None</code> for the <code>Distributor</code>.
 * Mix in this trait into any suite whose nested suites need to be run sequentially even with the rest of the
 * run is being executed concurrently.
 */
trait SequentialNestedSuiteExecution extends AbstractSuite { this: Suite =>

  /**
   * This trait's implementation of <code>runNestedSuites</code>s invokes <code>runNestedSuites</code> on <code>super</code>,
   * passing in <code>None</code> for the <code>Distributor</code>.
   *
   * @param args the <code>RunArgs</code> for this run
   *
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   */
  abstract override protected def runNestedSuites(args: RunArgs) {
    if (args == null)
      throw new NullPointerException("args was null")

    super.runNestedSuites(args)
  }
}
