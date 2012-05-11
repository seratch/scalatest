package org.scalatest.integ

object TestStatus extends Enumeration {
  type TestStatus = Value
  val STARTED, SUCCEEDED, FAILED, IGNORED, PENDING, CANCELED = Value
}

object ScopeStatus extends Enumeration {
  type ScopeStatus = Value
  val OPENED, CLOSED = Value
}

object SuiteStatus extends Enumeration {
  type SuiteStatus = Value
  val STARTED, SUCCEED, FAILED, ABORTED = Value
}

object RunStatus extends Enumeration {
  type RunStatus = Value
  val STARTED, COMPLETED, STOPPED, ABORTED = Value
}