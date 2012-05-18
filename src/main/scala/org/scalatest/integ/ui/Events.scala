package org.scalatest.integ.ui

import org.scalatest.integ.Node
import org.scalatest.integ.TestModel

case class NextFailureEvent

case class PreviousFailureEvent

case class TreeSelectedEvent(selected: Node, hasNextFailure: Boolean, hasPreviousFailure: Boolean)

case class ShowFailureOnlyEvent(failureOnly: Boolean)

case class RerunAllEvent

case class RerunFailedEvent(failedTestList: List[TestModel])

case class StopRunEvent