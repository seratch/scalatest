package org.scalatest.integ.spi.impl

import org.scalatest.integ.spi.StackTraceActionProvider
import java.awt.event.KeyEvent
import org.scalatest.integ.Node
import java.awt.event.MouseEvent
import org.scalatest.integ.ui.ResultController

class DefaultStackTraceActionProvider(controller: ResultController) extends StackTraceActionProvider {

  def stackTraceString(element: org.scalatest.integ.StackTraceElement): String = element.toString
  
  def mousePressed(event: MouseEvent, node: Node) {
    
  }
  
  def keyPressed(event: KeyEvent, node: Node) {
    
  }
  
}