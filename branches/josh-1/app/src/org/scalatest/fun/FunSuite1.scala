package org.scalatest.fun

abstract class FunSuite1[F] extends FunSuite {

  private case class Test(msg: String, f: (F) => Unit)

  private var fixtureTestsMap: Map[String, Test] = Map()

  override def testNames: Set[String] = {
    super.testNames ++ fixtureTestsMap.keySet
  }

  protected def withFixture(f: F => Unit) // this must be abstract

  protected def testWithFixture(msg: String)(f: F => Unit) {
    fixtureTestsMap = fixtureTestsMap + (msg -> Test(msg, f))
  }

  protected def specifyWithFixture(msg: String)(f: F => Unit) {
    fixtureTestsMap = fixtureTestsMap + (msg -> Test(msg, f))
  }

  protected override def runTest(testName: String, reporter: Reporter, stopper: Stopper, properties: Map[String, Any]) {

    if (testName == null || reporter == null || stopper == null || properties == null)
      throw new NullPointerException

    if (fixtureTestsMap.keySet.contains(testName)) {
      val wrappedReporter = reporter
  
      val report = new Report(testName, this.getClass.getName)
  
      wrappedReporter.testStarting(report)
  
      try {
        withFixture(fixtureTestsMap(testName).f)
  
        val report = new Report(getTestNameForReport(testName), this.getClass.getName)
  
        wrappedReporter.testSucceeded(report)
      }
      catch { 
        case e: Exception => {
          handleFailedTest(e, false, testName, None, wrappedReporter)
        }
        case ae: AssertionError => {
          handleFailedTest(ae, false, testName, None, wrappedReporter)
        }
      }
    }
    else {
      super.runTest(testName, reporter, stopper, properties)
    }
  }

  private def handleFailedTest(t: Throwable, hasPublicNoArgConstructor: Boolean, testName: String,
      rerunnable: Option[Rerunnable], reporter: Reporter) {

    val msg =
      if (t.getMessage != null) // [bv: this could be factored out into a helper method]
        t.getMessage
      else
        t.toString

    val report = new Report(getTestNameForReport(testName), msg, Some(t), None)

    reporter.testFailed(report)
  }
}