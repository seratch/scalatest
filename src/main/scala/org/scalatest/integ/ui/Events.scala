package org.scalatest.integ.ui

import org.scalatest.integ.Node

case class NextFailureEvent

case class PreviousFailureEvent

case class TreeSelectedEvent(selected: Node, hasNextFailure: Boolean, hasPreviousFailure: Boolean)