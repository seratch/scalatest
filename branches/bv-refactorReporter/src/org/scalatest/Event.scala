package org.scalatest

import java.util.Date

/**
 * A base class for the events that can be passed to the report function passed
 * to the <code>execute</code> method of a <code>Suite</code>.
 *
 * Will have a sealed abstract InfoProvided message with three final concrete subclasses,
 * RunInfoProvided, SuiteInfoProvided, TestInfoProvided. Anything that starts with Run just
 * has runStamp and ordinal; Suite has those plus suiteStamp; Test has those plus testStamp.
 */
sealed abstract class Event extends Ordered[Event] {

  val ordinal: Ordinal
  val formatter: Option[Formatter]
  val payload: Option[Any]
  val threadName: String
  val timeStamp: Long

  /**
   * Comparing <code>this</code> event with the event passed as <code>that</code>. Returns
   * x, where x < 0 iff this < that, x == 0 iff this == that, x > 0 iff this > that.
   *
   * @param that the event to compare to this event
   * @param return an integer indicating whether this event is less than, equal to, or greater than
   * the passed event
   */
  def compare(that: Event): Int = ordinal.compare(that.ordinal)
}

/**
 * Event that indicates a suite (or other entity) is about to start running a test.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestStarting</code> to report
 * that a test method of a <code>Suite</code> is about to be invoked.
 * </p>
 *
 * <p>
 * This class has a private constructor. To create instances of this class you must
 * use one of the factory methods provided in its <a href="TestStarting$object.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestStarting</code> event like this:
 * </p>
 *
 * <pre>
 * report(TestStarting(userFriendlyName, suiteName, thisSuite.getClass.getName, testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param name a localized name identifying the test that is starting, which should include the
 *        suite and test names, suitable for presenting to the user
 * @param suiteName the name of the suite containing the test that is starting
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
 * @param testName the name of the test that is starting
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that is starting (if <code>None</code>
 *        is passed, the test cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the test starting event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 */
final case class TestStarting private (
  ordinal: Ordinal,
  name: String,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  formatter: Option[Formatter],
  rerunnable: Option[Rerunnable],
  payload: Option[Any],
  threadName: String,
  timeStamp: Long
) extends Event {
    
  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (name == null)
    throw new NullPointerException("name was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (rerunnable == null)
    throw new NullPointerException("rerunnable was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Companion object for the <a href="TestStarting.html"><code>TestStarting</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>TestStarting</code> objects.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 */
object TestStarting {

  /**
   * Constructs a new <code>TestStarting</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that is starting, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that is starting
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
   * @param testName the name of the test that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that is starting (if <code>None</code>
   *        is passed, the test cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the test starting event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter],
    rerunnable: Option[Rerunnable],
    payload: Option[Any]
  ): TestStarting = {
    apply(ordinal, name, suiteName, suiteClassName, testName, formatter, rerunnable, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestStarting</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that is starting, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that is starting
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
   * @param testName the name of the test that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that is starting (if <code>None</code>
   *        is passed, the test cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter],
    rerunnable: Option[Rerunnable]
  ): TestStarting = {
    apply(ordinal, name, suiteName, suiteClassName, testName, formatter, rerunnable, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestStarting</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunnable</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that is starting, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that is starting
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
   * @param testName the name of the test that is starting
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter]
  ): TestStarting = {
    apply(ordinal, name, suiteName, suiteClassName, testName, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestStarting</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunnable</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that is starting, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that is starting
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that is starting
   * @param testName the name of the test that is starting
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestStarting</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String
  ): TestStarting = {
    apply(ordinal, name, suiteName, suiteClassName, testName, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Event that indicates a suite (or other entity) has completed running a test that succeeded.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestSucceeded</code> to report
 * that a test method of a <code>Suite</code> returned normally
 * (without throwing an <code>Exception</code>).
 * </p>
 *
 * <p>
 * This class has a private constructor. To create instances of this class you must
 * use one of the factory methods provided in its <a href="TestSucceeded$object.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestSucceeded</code> event like this:
 * </p>
 *
 * <pre>
 * report(TestSucceeded(userFriendlyName, suiteName, thisSuite.getClass.getName, testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param name a localized name identifying the test that has succeeded, which should include the
 *        suite and test names, suitable for presenting to the user
 * @param suiteName the name of the suite containing the test that has succeeded
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
 * @param testName the name of the test that has succeeded
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that has succeeded (if <code>None</code>
 *        is passed, the test cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the test starting event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 */
final case class TestSucceeded private (
  ordinal: Ordinal,
  name: String,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  formatter: Option[Formatter],
  rerunnable: Option[Rerunnable],
  payload: Option[Any],
  threadName: String,
  timeStamp: Long
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (name == null)
    throw new NullPointerException("name was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (rerunnable == null)
    throw new NullPointerException("rerunnable was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Companion object for the <a href="TestSucceeded.html"><code>TestSucceeded</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>TestSucceeded</code> objects.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 */
object TestSucceeded {

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that has succeeded, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that has succeeded (if <code>None</code>
   *        is passed, the test cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the test starting event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter],
    rerunnable: Option[Rerunnable],
    payload: Option[Any]
  ): TestSucceeded = {
    apply(ordinal, name, suiteName, suiteClassName, testName, formatter, rerunnable, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that has succeeded, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that has succeeded (if <code>None</code>
   *        is passed, the test cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter],
    rerunnable: Option[Rerunnable]
  ): TestSucceeded = {
    apply(ordinal, name, suiteName, suiteClassName, testName, formatter, rerunnable, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunnable</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that has succeeded, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    formatter: Option[Formatter]
  ): TestSucceeded = {
    apply(ordinal, name, suiteName, suiteClassName, testName, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestSucceeded</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunnable</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that has succeeded, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has succeeded
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has succeeded
   * @param testName the name of the test that has succeeded
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestSucceeded</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String
  ): TestSucceeded = {
    apply(ordinal, name, suiteName, suiteClassName, testName, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

/**
 * Event that indicates a suite (or other entity) has completed running a test that failed.
 *
 * <p>
 * For example, trait <code>Suite</code> uses <code>TestFailed</code> to report
 * that a test method of a <code>Suite</code> completed abruptly with an <code>Exception</code>.
 * </p>
 *
 * <p>
 * This class has a private constructor. To create instances of this class you must
 * use one of the factory methods provided in its <a href="TestFailed$object.html">companion object</a>. For example, given a
 * report function named <code>report</code>, you could fire a <code>TestFailed</code> event like this:
 * </p>
 *
 * <pre>
 * report(TestFailed(userFriendlyName, suiteName, thisSuite.getClass.getName, testName))
 * </pre>
 *
 * <p>
 * The suite class name parameter is optional, because suites in ScalaTest are an abstraction that
 * need not necessarily correspond to one class. Nevertheless, it most cases each suite will correspond
 * to a class, and when it does, the fully qualified name of that class should be reported by passing a
 * <code>Some</code> for <code>suiteClassName</code>. One use for this bit of information is JUnit integration,
 * because the "name" provided to a JUnit <code>org.junit.runner.Description</code> appears to usually include
 * a fully qualified class name by convention.
 * </p>
 *
 * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
 *        other events reported during the same run
 * @param name a localized name identifying the test that has failed, which should include the
 *        suite and test names, suitable for presenting to the user
 * @param suiteName the name of the suite containing the test that has failed
 * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
 * @param testName the name of the test that has failed
 * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
 *        or a <code>Throwable</code> created to capture stack trace information about the problem.
 *     is reported without describing a <code>Throwable</code>.
 * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
 *        how to present this event to the user
 * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that has failed (if <code>None</code>
 *        is passed, the test cannot be rerun)
 * @param payload an optional object that can be used to pass custom information to the reporter about the test starting event
 * @param threadName a name for the <code>Thread</code> about whose activity this event was reported
 * @param timeStamp a <code>Long</code> indicating the time this event was reported, expressed in terms of the
 *        number of milliseconds since the standard base time known as "the epoch":  January 1, 1970, 00:00:00 GMT
 */
final case class TestFailed private (
  ordinal: Ordinal,
  name: String,
  suiteName: String,
  suiteClassName: Option[String],
  testName: String,
  throwable: Option[Throwable],
  formatter: Option[Formatter],
  rerunnable: Option[Rerunnable],
  payload: Option[Any],
  threadName: String,
  timeStamp: Long
) extends Event {

  if (ordinal == null)
    throw new NullPointerException("ordinal was null")
  if (name == null)
    throw new NullPointerException("name was null")
  if (suiteName == null)
    throw new NullPointerException("suiteName was null")
  if (suiteClassName == null)
    throw new NullPointerException("suiteClassName was null")
  if (testName == null)
    throw new NullPointerException("testName was null")
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  if (formatter == null)
    throw new NullPointerException("formatter was null")
  if (rerunnable == null)
    throw new NullPointerException("rerunnable was null")
  if (payload == null)
    throw new NullPointerException("payload was null")
  if (threadName == null)
    throw new NullPointerException("threadName was null")
}

/**
 * Companion object for the <a href="TestFailed.html"><code>TestFailed</code></a> event, which contains overloaded factory methods
 * and an extractor method to facilitate pattern matching on <code>TestFailed</code> objects.
 *
 * <p>
 * All factory methods throw <code>NullPointerException</code> if any of the passed values are <code>null</code>.
 * </p>
 */
object TestFailed {

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing the current thread's
   * name as <code>threadname</code> and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that has failed, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param testName the name of the test that has failed
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that has failed (if <code>None</code>
   *        is passed, the test cannot be rerun)
   * @param payload an optional object that can be used to pass custom information to the reporter about the test starting event
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable],
    formatter: Option[Formatter],
    rerunnable: Option[Rerunnable],
    payload: Option[Any]
  ): TestFailed = {
    apply(ordinal, name, suiteName, suiteClassName, testName, throwable, formatter, rerunnable, payload, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing <code>None</code> as the
   * <code>payload</code>, the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that has failed, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param testName the name of the test that has failed
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   * @param rerunnable an optional <code>Rerunnable</code> that can be used to rerun the test that has failed (if <code>None</code>
   *        is passed, the test cannot be rerun)
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable],
    formatter: Option[Formatter],
    rerunnable: Option[Rerunnable]
  ): TestFailed = {
    apply(ordinal, name, suiteName, suiteClassName, testName, throwable, formatter, rerunnable, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing <code>None</code> as the
   * <code>rerunnable</code>, <code>None</code> as the <code>payload</code>, the current threads name as <code>threadname</code>,
   * and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that has failed, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param testName the name of the test that has failed
   * @param formatter an optional formatter that provides extra information that can be used by reporters in determining
   *        how to present this event to the user
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable],
    formatter: Option[Formatter]
  ): TestFailed = {
    apply(ordinal, name, suiteName, suiteClassName, testName, throwable, formatter, None, None, Thread.currentThread.getName, (new Date).getTime)
  }

  /**
   * Constructs a new <code>TestFailed</code> event with the passed parameters, passing <code>None</code> for
   * <code>formatter</code>, <code>None</code> as the <code>rerunnable</code>, <code>None</code> as the <code>payload</code>,
   * the current threads name as <code>threadname</code>, and the current time as <code>timeStamp</code>.
   *
   * @param ordinal an <code>Ordinal</code> that can be used to place this event in order in the context of
   *        other events reported during the same run
   * @param name a localized name identifying the test that has failed, which should include the
   *        suite and test names, suitable for presenting to the user
   * @param suiteName the name of the suite containing the test that has failed
   * @param suiteClassName an optional fully qualifed <code>Suite</code> class name containing the test that has failed
   * @param testName the name of the test that has failed
   * @param throwable an optional <code>Throwable</code> that, if a <code>Some</code>, indicates why the test has failed,
   *        or a <code>Throwable</code> created to capture stack trace information about the problem.
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>
   * @return a new <code>TestFailed</code> instance initialized with the passed and default values
   */
  def apply(
    ordinal: Ordinal,
    name: String,
    suiteName: String,
    suiteClassName: Option[String],
    testName: String,
    throwable: Option[Throwable]
  ): TestFailed = {
    apply(ordinal, name, suiteName, suiteClassName, testName, throwable, None, None, None, Thread.currentThread.getName, (new Date).getTime)
  }
}

