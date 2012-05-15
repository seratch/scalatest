package org.scalatest.integ.spi

import java.awt.event.MouseEvent
import org.scalatest.integ.Node
import java.awt.event.KeyEvent

trait StackTraceActionProvider {
  def stackTraceString(element: org.scalatest.integ.StackTraceElement): String
  
  def mousePressed(event: MouseEvent, node: Node)
  
  def keyPressed(event: KeyEvent, node: Node)
  
}