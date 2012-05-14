package org.scalatest.integ.spi

import org.scalatest.integ.Node
import java.awt.event.MouseEvent
import java.awt.event.KeyEvent

trait TreeActionProvider {
  
  def mousePressed(event: MouseEvent, node: Node)
  
  def keyPressed(event: KeyEvent, node: Node)
}