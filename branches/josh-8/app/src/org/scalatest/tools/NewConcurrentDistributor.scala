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

import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.Future

/**
 * This Distributor can be used by multiple threads.
 *
 * @author Bill Venners
 * @author Josh Cough
 */
private[scalatest] class NewConcurrentDistributor(scalatest: ScalaTest) extends Distributor {

  private val execSvc: ExecutorService = Executors.newCachedThreadPool()
  private val futureQueue = new LinkedBlockingQueue[Future[T] forSome { type T }]

  def put(suite: Suite) = {
    val suiteRunner = new Runnable(){
      override def run(){
        scalatest.run(suite)
      }
    }
    val future: Future[_] = execSvc.submit(suiteRunner)
    futureQueue.put(future)
  }
  
  def putAll( suites: List[Suite]) = {
    suites.foreach( suite => this.put(suite) )
  }

  def poll() = None

  def waitUntilDone(): Unit = {
    while (futureQueue.peek != null)
      futureQueue.poll().get()
  }
}