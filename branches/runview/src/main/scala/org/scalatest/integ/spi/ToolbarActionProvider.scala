package org.scalatest.integ.spi

import java.awt.event.ActionEvent
import java.awt.event.ItemEvent

trait ToolbarActionProvider {
  
  def nextFailure(e: ActionEvent)
  
  def previousFailure(e: ActionEvent)
  
  def showFailureOnly(e: ItemEvent, failureOnly: Boolean)
}