/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest.fixture

import org.scalatest._

/**
 * Trait that when mixed into a <code>fixture.Suite</code> passes the
 * <code>TestData</code> passed to <code>withFixture</code> as a fixture into each test.
 *
 * @author Bill Venners
 */
trait TestDataFixture { this: fixture.Suite =>

  /**
   * The type of the fixture, which is <code>TestData</code>.
   */
  type FixtureParam = TestData

  /**
   * Invoke the test function, passing to the the test function to itself, because
   * in addition to being the test function, it is the <code>TestData</code> for the test.
   *
   * <p>
   * To enable stacking of traits that define <code>withFixture(NoArgTest)</code>, this method does not
   * invoke the test function directly. Instead, it delegates responsibility for invoking the test function
   * to <code>withFixture(NoArgTest)</code>.
   * </p>
   *
   * @param test the <code>OneArgTest</code> to invoke, passing in the
   *   <code>TestData</code> fixture
   */
  def withFixture(test: OneArgTest) {
    withFixture(test.toNoArgTest(test))
  }
}
