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
 * A <code>Reporter</code> that prints test status information to
 * the standard error stream.
 *
 * @author Bill Venners
 */
private[scalatest] class StandardErrReporter extends PrintReporter(Console.err) {

  /**
   * Does nothing, because don't want to dispose the standard error stream.
   */
  override def dispose() {
  }
}