package org.scalatest.integ.spi

import org.scalatest.integ.Node
import java.awt.event.MouseEvent
import java.awt.event.KeyEvent
import javax.swing.event.TreeSelectionEvent

trait TreeActionProvider {
  
  def mousePressed(event: MouseEvent, node: Node)
  
  def keyPressed(event: KeyEvent, node: Node)
  
  def selectionChanged(event: TreeSelectionEvent, node: Node)
}