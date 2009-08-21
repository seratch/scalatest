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
package org.scalatest.concurrent

import org.scalatest._
import Thread.State._
import PimpedThreadGroup._
import _root_.java.util.concurrent._
import _root_.java.util.concurrent.atomic.AtomicReference
import org.scalatest.Informer

/**
 * Class that facilitates the testing of concurrency abstractions.
 *
 * <p>
 * A <code>Conductor</code> conducts a multi-threaded test by maintaining
 * a clock of "beats." Beats are numbered starting with 0. You can ask a
 * <code>Conductor</code> to run threads that interact with the concurrency
 * abstraction you want to test. A thread can call the <code>Conductor</code>'s
 * <code>waitForBeat</code> method, which will cause the thread to block
 * until that beat has been reached. The <code>Conductor</code> will advance
 * the beat only when all threads participating in the test are blocked. By
 * tying the timing of thread activities to specific beats, you can write
 * tests for concurrency abstractions that have specific interleavings of
 * threads.
 * </p>
 *
 * <p>
 * A <code>Conductor</code> object has a three-phase lifecycle. It begins its life
 * in the <em>setup</em> phase. During this phase, you can start threads by
 * invoking the <code>thread</code> method on the <code>Conductor</code>.
 * When <code>conductTest</code> is invoked on a <code>Conductor</code>, it enters
 * the <em>conducting</em> phase. During this phase it conducts the one multi-threaded
 * test it was designed to conduct. After all participating threads have exited, either by
 * returning normally or throwing an exception, the <code>conductTest</code> method
 * will complete, either by returning normally or throwing an exception. As soon as
 * the <code>conductTest</code> method completes, the <code>Conductor</code>
 * enters its <em>defunct</em> phase. Once the <code>Conductor</code> has conducted
 * a multi-threaded test, it is defunct and can't be reused. To run the same test again,
 * you'll need to create a new instance of <code>Conductor</code>.
 * </p>
 *
 * <p>
 * Class <code>Conductor</code> was inspired by the
 * <a href="http://www.cs.umd.edu/projects/PL/multithreadedtc/">MultithreadedTC project</a>,
 * created by Bill Pugh and Nat Ayewah of the University of Maryland.
 * </p>
 *
 * @author Josh Cough
 * @author Bill Venners
 */
class Conductor(val informer: Option[Informer]){

  def this() = this(None)

  /**
   * The metronome used to coordinate between threads.
   * This clock is advanced by the clock thread.
   * The clock will not advance if it is frozen.
   */
  private val clock = new Clock

  /////////////////////// thread management start //////////////////////////////

  // place all threads in a new thread group
  private val threadGroup = new ThreadGroup("Orchestra")

  // all the threads in this test
  private val threads = new CopyOnWriteArrayList[Thread]()

  // the main test thread
  private val mainThread = currentThread

  /**
   * Creates a new thread that will execute the given function.
   *
   * @param fun the function to be executed by the thread
   * @return the newly created thread
   */
  def thread[T](fun: => T): Thread = thread("thread" + threads.size) { fun }

  /*
   * Create a new thread that will execute the given Runnable
   * @param runnable the Runnable to be executed by the thread
   */
  // def thread[T](runnable: Runnable): Thread = thread("thread" + threads.size) {runnable.run}

  /*
   * Create a new thread that will execute the given Runnable
   * @param runnable the Runnable to be executed by the thread
   */
  // def thread[T](name: String, runnable: Runnable): Thread = thread(name) {runnable.run}

  /*
   * Create a new thread that will execute the given Callable
   * @param callable the Callable to be executed by the thread
   */
  // def thread[T](callable: Callable[T]): Thread = thread("thread" + threads.size) {callable.call}

  /*
   * Create a new thread that will execute the given Callable
   * @param callable the Callable to be executed by the thread
   */
  // def thread[T](name: String, callable: Callable[T]): Thread = thread(name) {callable.call}

  /**
   * Creates a new thread that will execute the given function.
   *
   * @param name the name of the thread
   * @param fun the function to be executed by the thread
   * @return the newly created thread
   */
  def thread[T](name: String)(fun: => T): Thread = {
    currentState.get match {
      case TestFinished => throw new IllegalStateException("Test already completed.")
      case _ =>
        val t = TestThread(name, fun _)
        threads add t
        startThread(t)
    }
  }
  
  // The reason that the thread is started immediately, is do that nested threads
  // will start immediately, without requiring the user to explicitly start() them.

  /*
   * Adds threads methods to int, so one can say:<br/>
   * val threads:List[Thread] = 5.threads("some name"){ ... }<br/>
   * val anonymous_threads:List[Thread] = 10 threads { ... }<br/>
   * @param nrThreads the number of threads to be created
   */

  /*
  private implicit def addThreadsMethodToInt(nrThreads:Int) = new ThreadedInt(nrThreads)

  private class ThreadedInt(nrThreads:Int) {
    def threads[T](name: String)(f: => T): List[Thread] = {
      val seq = for( i <- 1 to nrThreads) yield thread(name + "("+i+")") {f}
      seq.toList
    }
    def threads[T](f: => T): List[Thread] = {
      val seq = for( i <- 1 to nrThreads) yield thread{f}
      seq.toList
    }
  }
*/
  
  /**
   * A test thread runs the given function.
   * It only does so after it is given permission to do so by the main thread.
   * The main thread grants permission after it receives notication that
   * all test threads are ready to go.
   */
  private case class TestThread[T](name: String, f: () => T) extends Thread(threadGroup, name){
    override def run() {
      try {
        // notify the main thread that we are indeed ready to go.
        mainThreadStartLatch.countDown
        // wait for the main thread to say its ok to go.
        testThreadStartLatch.await
        // go
        f()
      } catch {
        // The reason this is a catch Throwable is because you want to let ThreadDeath through
        // without signalling errors. Otherwise the signalError could have been in a finally.
        // If the simulation is aborted, then stop will be called,
        // which will cause ThreadDeath, so just die and do nothing
        case e: ThreadDeath =>
        case t: Throwable => signalError(t)
      }
    }
  }

  /**
   * Starts a thread, logging before and after
   */
  private def startThread(thread: Thread): Thread = {
    logAround("starting: " + thread) {
      thread.start()
      thread
    }
  }

  /////////////////////// thread management end /////////////////////////////

  /////////////////////// error handling start //////////////////////////////

  /**
   * a BlockingQueue containing the first Error/Exception that occured
   * in thread methods or that are thrown by the clock thread
   */
  private val errorsQueue = new ArrayBlockingQueue[Throwable](20)

  /*
   * A list of any errors thrown by test threads at the time this method is called.
   */
  /*
  def exceptions: List[Throwable] = {
    def exceptions(errorList: List[Throwable], it: java.util.Iterator[Throwable]): List[Throwable] = {
      if(it.hasNext) exceptions( errorList ::: List(it.next), it)
      else errorList
    }
    exceptions(Nil, errorsQueue.iterator)
  }
    */

  /**
   * Stop all test case threads and clock thread, except the thread from
   * which this method is called. This method is used when a thread is
   * ready to end in failure and it wants to make sure all the other
   * threads have ended before throwing an exception.
   * Clock thread will return normally when no threads are running.
   */
  private def signalError(t: Throwable) {
    log(t)
    errorsQueue offer t
    for (t <- threadGroup.getThreads; if (t != currentThread)) {
      log("signaling error to " + t.getName)
      val assertionError = new AssertionError(t.getName + " killed by " + currentThread.getName)
      assertionError setStackTrace t.getStackTrace
      t stop assertionError
    }
  }

  /////////////////////// error handling end //////////////////////////////

  /**
   * Invokes <code>conductTest</code> and after <code>conductTest</code> returns,
   * if <code>conductTest</code> returns normally (<em>i.e.</em>, without throwing
   * an exception), invokes the passed function.
   *
   * <p>
   * If <code>conductTest</code> completes abruptly with an exception, this method
   * will complete abruptly with the same exception and not execute the passed
   * function.
   * </p>
   *
   * @param fun the function to execute after <code>conductTest</code> test returns
   */
  def whenFinished(fun: => Unit) {
    if(currentThread != mainThread)
      throw new IllegalStateException("whenFinished can only be called by thread that created Conductor.")

    conductTest()

    fun
  }

  /////////////////////// clock management start //////////////////////////

  /**
   * Blocks the current thread until the thread beat reaches the
   * specified value, at which point the current thread will be unblocked.
   *
   * @param beat the tick value to wait for
   */
  def waitForBeat(beat: Int) { clock waitForBeat beat }

  /**
   * The current value of the thread clock.
   *
   * @return the current beat value
   */
  def beat: Int = clock.currentBeat

  /**
   * Executes the passed function with the <code>Conductor</code> <em>frozen</em> so that it
   * won't advance the clock.
   *
   * <p>
   * While the <code>Conductor</code> is frozen, the beat will not advance. Once the
   * passed function has completed executing, the <code>Conductor</code> will be unfrozen
   * so that the beat will advance when all threads are blocked as normal.
   * </p>
   *
   * @param fun the function to execute while the <code>Conductor</code> is frozen.
   */
  def withConductorFrozen[T](fun: => T) = clock.withClockFrozen(fun _)

  /**
   * Indicates whether the conductor has been frozen.
   *
   * <p>
   * Note: The only way a thread
   * can freeze the conductor is by calling <code>withConductorFrozen</code>.
   * </p>
   */
  def isConductorFrozen: Boolean = clock.isFrozen

  /////////////////////// clock management end //////////////////////////////

  /////////////////////// run methods start /////////////////////////////////

  /**
   * Keeps the main thread from allowing the test threads to execute their bodies
   * until all of them are started, and ready to go.
   */
  private lazy val mainThreadStartLatch = new CountDownLatch(threads.size)

  /**
   * Keeps the test threads from executing their bodies until the main thread
   * allows them to.
   */
  private val testThreadStartLatch = new CountDownLatch(1)

  /**
   * Conducts a multithreaded test with a default clock period of 10 milliseconds
   * and default run limit of 5 seconds.
   */
  def conductTest() {
    val DEFAULT_CLOCKPERIOD = 10
    val DEFAULT_RUNLIMIT = 5
    conductTest(DEFAULT_CLOCKPERIOD, DEFAULT_RUNLIMIT)
  }

  private val currentState: AtomicReference[ConductorState] = new AtomicReference(Setup)

  /**
   * Indicates whether either of the two overloaded <code>conductTest</code> methods
   * have been invoked.
   */
  def conductTestWasCalled = currentState.get.testWasStarted

  /**
   * Conducts a multithreaded test with the specified clock period (in milliseconds)
   * and run limit (in seconds).
   *
   * @param clockPeriod The period (in ms) between checks for the clock
   * @param runLimit The limit to run the test in seconds
   * @throws Throwable The first error or exception that is thrown by one of the threads
   */
  def conductTest(clockPeriod: Int, runLimit: Int) {

    // if the test was started already, explode
    // otherwise, change state to TestStarted
    if( conductTestWasCalled ) throw new IllegalStateException("Conductor can only be run once!")
    else currentState set TestStarted

    // wait until all threads are definitely ready to go
    mainThreadStartLatch.await()

    // release the latch, allowing all threads to start
    // wait for all the test threads to start before starting the clock
    testThreadStartLatch.countDown()

    // start the clock thread
    val clockThread = startThread(ClockThread(clockPeriod, runLimit))

    // wait until all threads have ended
    waitForThreads

    // change state to test finished
    currentState set TestFinished
  }

  /**
   * Wait for all of the test case threads to complete, or for one
   * of the threads to throw an exception, or for the clock thread to
   * interrupt this (main) thread of execution. When the clock thread
   * or other threads fail, the error is placed in the shared error array
   * and thrown by this method.
   *
   * @param threads List of all the test case threads and the clock thread
   */
  // Explain how we understand it works: if the thread that's been joined already dies with an exception
  // that will go into errors, and this thread the join will return. If the thread returns and doesn't
  // die, that means all went well, and join will return and it can loop to the next one.
  // There should be no race condition between the last thread being waited on by join, it dies, join
  // returns, and after that the error gets into the errors. Because if you look in run() in the
  // thread inside createTestThread, the signalling error happens in a catch Throwable block before the thread
  // returns.
  private def waitForThreads{
    while(threadGroup.anyThreadsAlive_?){
      threadGroup.getThreads foreach waitForThread
    }
  }

  private def waitForThread(t: Thread) {
    log("waiting for: " + t.getName + " which is in state:" + t.getState)
    try {
      if (t.isAlive && !errorsQueue.isEmpty) logAround("stopping: " + t) {t.stop()}
      else logAround("joining: " + t) {t.join()}
      assert(t.getState == TERMINATED)
    } catch {
      case e: InterruptedException => {
        log("killed waiting for threads. probably deadlock or timeout.")
        errorsQueue offer new AssertionError(e)
      }
    }
  }

  private def log(a: Any) = {
    informer match {
      case Some(info) => info(a.toString)
      case None => 
    }
  }

  /**
   * Logs before and after executing the given function.
   */
  private def logAround[T](msg: String)(f: => T): T = {
    log("|starting: " + msg)
    val t = f
    log("|done with: " + msg)
    t
  }

  /////////////////////// logging end /////////////////////////////


  /**
   * A Clock manages the current beat in a MultiThreadedTest.
   * Several duties stem from that responsibility.
   *
   * The clock will:
   *
   * <ol>
   * <li>Block a thread until the tick has reached a particular time.</li>
   * <li>Report the current time</li>
   * <li>Run operations with the clock frozen.</li>
   * </ol>
   */
  private class Clock {

    import java.util.concurrent.locks.ReentrantReadWriteLock
    import PimpedReadWriteLock._

    // clock starts at time 0
    private var currentTime = 0
    private val lock = new AnyRef

    /**
     * Read locks are acquired when clock is frozen and must be
     * released before the clock can advance in a waitForBeat().
     */
    private val rwLock = new ReentrantReadWriteLock

    private var highestBeatBeingWaitedOn = 0

    /**
     * Advance the current tick. In order to do so, the clock will wait
     * until it has become unfrozen.
     *
     * All threads waiting for the clock to tick will be notified after the advance.
     *
     * Only the clock thread should be calling this.
     *
     * If the clock has been frozen by a thread, then that thread will own the readLock. Write
     * lock can only be acquired when there are no readers, so ticks won't progress while someone
     * has the clock frozen. Other methods also grab the read lock, like time (which gets
     * the current tick.)
     */
    def advance() {
      lock.synchronized {
        rwLock.write {
          log("clock advancing from: " + currentTime + " to: " + (currentTime + 1))
          currentTime += 1
        }
        lock.notifyAll()
      }
    }

    /**
     * The current beat.
     */
    def currentBeat: Int = rwLock read currentTime

    /**
     * When wait for tick is called, the current thread will block until
     * the given tick is reached by the clock.
     */
    // TODO: Could just notify in the advance() method the folks that are waiting on that
    // particular beat, but then that's more complicated. Not a big deal.
    def waitForBeat(beat: Int) {
      lock.synchronized {
        if (beat > highestBeatBeingWaitedOn) highestBeatBeingWaitedOn = beat
        logAround(currentThread.getName + " is waiting for time " + beat) {
          while (currentBeat < beat) {
            try {
              lock.wait()
            } catch {
              case e: InterruptedException => throw new AssertionError(e)
            }
          }
        }
      }
    }
    // The reason there's no race condition between calling time() in the while and calling
    // lock.wait() later (between that) and some other thread incrementing the tick and doing
    // a notify that this thread would miss (which it would want to know about if that's the
    // new time that it's waiting for) is becauswe both this and the tick method are synchronized
    // on the lock.

    /**
     * Returns true if any thread is waiting for a tick in the future ( greater than the current time )
     */
    def isAnyThreadWaitingForABeat = {
      lock.synchronized {highestBeatBeingWaitedOn > currentTime}
    }

    /**
     * When the clock is frozen, it will not advance even when all threads
     * are blocked. Use this to block the current thread with a time limit,
     * but prevent the clock from advancing due to a    { @link # waitForTick ( int ) } in
     * another thread.
     */
    def withClockFrozen[T](f: => T): T = rwLock read f

    /**
     * Check if the clock has been frozen by any threads.
     */
    def isFrozen: Boolean = rwLock.getReadLockCount > 0
  }

  /**
   * The clock thread is the manager of the MultiThreadedTest.
   * Periodically checks all the test case threads and regulates them.
   * If all the threads are blocked and at least one is waiting for a tick,
   * the clock advances to the next tick and the waiting thread is notified.
   * If none of the threads are waiting for a tick or in timed waiting,
   * a deadlock is detected. The clock thread times out if a thread is in runnable
   * or all are blocked and one is in timed waiting for longer than the runLimit.
   *
   * Algorithm in detail:
   *
   * While there are threads alive
   *
   *    If there are threads RUNNING
   *
   *       If they have been running too long
   *
   *          stop the test with a timeout error
   *
   *    else if there are threads waiting for a clock tick
   *
   *       advance the clock
   *
   *    else if there are threads in TIMED_WAITING
   *
   *       increment the deadlock counter
   *
   *       if the deadlock counter has reached a threadshold
   *
   *          stop the test due to potential deadlock
   *
   *    sleep clockPeriod ms
   *
   *
   * @param mainThread The main test thread. This thread will be waiting
   * for all the test threads to finish. It will be interrupted if the
   * ClockThread detects a deadlock or timeout.
   *
   * @param clockPeriod The period (in ms) between checks for the clock
   *
   * @param maxRunTime The limit to run the test in seconds
   */
  private case class ClockThread(clockPeriod: Int, maxRunTime: Int) extends Thread("Clock") {
    this setDaemon true // TODO: Why is this a daemon thread? If no good reason, drop it.

    // used in detecting timeouts
    private var lastProgress = System.currentTimeMillis

    // used in detecting deadlocks
    private var deadlockCount = 0
    private val MAX_DEADLOCK_DETECTIONS_BEFORE_DEADLOCK = 50

    /**
     * Runs the steps described above.
     */
    override def run {
      while (threadGroup.anyThreadsAlive_?) {
        if (threadGroup.anyThreadsRunning_?) {
          if (runningTooLong_?) timeout()
        }
        else if (clock.isAnyThreadWaitingForABeat) {
          clock.advance()
          deadlockCount = 0
          lastProgress = System.currentTimeMillis
        }
        else if (!threadGroup.anyThreadsInTimedWaiting_?) {
          detectDeadlock()
        }
        Thread sleep clockPeriod
      }
    }

    /**
     * Threads have been running too long (timeout) if
     * The number of seconds since the last progress are more
     * than the allowed maximum run time.
     */
    private def runningTooLong_? = System.currentTimeMillis - lastProgress > 1000L * maxRunTime

    /**
     * Stop the test tue to a timeout.
     */
    private def timeout() {
      val errorMessage = "Timeout! Test ran longer than " + maxRunTime + " seconds."
      signalError(new IllegalStateException(errorMessage))
      mainThread.interrupt()
    }

    /**
     * Determine if there is a deadlock and if so, stop the test.
     */
    private def detectDeadlock() {
      if (deadlockCount == MAX_DEADLOCK_DETECTIONS_BEFORE_DEADLOCK) {
        val errorMessage = "Apparent Deadlock! Threads waiting 50 clock periods (" + (clockPeriod * 50) + "ms)"
        signalError(new IllegalStateException(errorMessage))
        mainThread.interrupt()
      }
      else deadlockCount += 1
    }
  }

  /**
   * Base class for the possible states of the Conductor.
   */
  private sealed case class ConductorState(testWasStarted:Boolean, testIsFinished: Boolean)

  /**
   * The initial state of the Conductor.
   * Any calls the thread{ ... } will result in blocked Threads.
   * Any call to conductTest will start the test.
   */
  private case object Setup extends ConductorState(false, false)

  /**
   * The state of the Conductor while its running.
   * Any calls the thread{ ... } will result in running Threads.
   * Any further call to conductTest will result in an exception.
   */
  private case object TestStarted extends ConductorState(true, false)

  /**
   * The state of the Conductor after all threads have finished,
   * and the whenFinished method has completed.
   * Any calls the thread{ ... } will result in an exception
   * Any call to conductTest will result in an exception.
   */
  private case object TestFinished extends ConductorState(true, true)
}
