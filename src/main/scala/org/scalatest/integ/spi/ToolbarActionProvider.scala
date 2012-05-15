package org.scalatest.integ.spi

import java.awt.event.ActionEvent

trait ToolbarActionProvider {
  
  def nextFailure(e: ActionEvent)
  
  def previousFailure(e: ActionEvent)
}