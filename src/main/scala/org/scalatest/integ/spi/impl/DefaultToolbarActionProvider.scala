package org.scalatest.integ.spi.impl

import org.scalatest.integ.spi.ToolbarActionProvider
import org.scalatest.integ.ui.ResultController
import java.awt.event.ActionEvent
import org.scalatest.integ.ui.NextFailureEvent
import org.scalatest.integ.ui.PreviousFailureEvent
import javax.swing.event.ChangeEvent
import org.scalatest.integ.ui.ShowFailureOnlyEvent
import java.awt.event.ItemEvent
import org.scalatest.integ.ui.RerunAllEvent

class DefaultToolbarActionProvider(controller: ResultController) extends ToolbarActionProvider {

  def nextFailure(e: ActionEvent) {
    controller.notifyChanges(new NextFailureEvent())
  }
  
  def previousFailure(e: ActionEvent) {
    controller.notifyChanges(new PreviousFailureEvent())
  }
  
  def showFailureOnly(e: ItemEvent, failureOnly: Boolean) {
    controller.notifyChanges(new ShowFailureOnlyEvent(failureOnly))
  }
  
  def rerunAll(e: ActionEvent) {
    controller.notifyChanges(new RerunAllEvent())
  }
}