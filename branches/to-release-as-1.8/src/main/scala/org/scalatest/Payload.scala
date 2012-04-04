/*
 * Copyright 2001-2012 Artima, Inc.
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
 * Trait implemented by exception types that carry an optional payload.
 *
 * <p>
 * Many ScalaTest events include an optional "payload" field that can be used
 * to pass information to a custom reporter. This trait facilitates such customization, 
 * by allowing test code to include a payload in an exception (such as <code>TestFailedException</code>).
 * ScalaTest looks for this trait and fires any payloads it finds in the relevant ScalaTest event
 * stimulated by the exception, such as a <code>TestFailed</code> event stimulated by a <code>TestFailedException</code>.
 * </p>
 *
 * @author Bill Venners
 */
trait Payload { this: Throwable =>
  
  /**
   * The optional payload.
   */
  val payload: Option[Any]
}

