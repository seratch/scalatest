package org.scalatest.integ.spi.impl

import org.scalatest.integ.spi.ToolbarActionProvider
import org.scalatest.integ.ui.ResultController
import java.awt.event.ActionEvent
import org.scalatest.integ.ui.NextFailureEvent
import org.scalatest.integ.ui.PreviousFailureEvent

class DefaultToolbarActionProvider(controller: ResultController) extends ToolbarActionProvider {

  def nextFailure(e: ActionEvent) {
    controller.notifyChanges(new NextFailureEvent())
  }
  
  def previousFailure(e: ActionEvent) {
    controller.notifyChanges(new PreviousFailureEvent())
  }
  
}