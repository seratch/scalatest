package org.scalatest.integ.spi.impl

import org.scalatest.integ.spi.TreeActionProvider
import org.scalatest.integ.TopOfClass
import org.scalatest.integ.TopOfMethod
import org.scalatest.integ.LineInFile
import org.scalatest.integ.SeeStackDepthException
import javax.swing.JOptionPane
import org.scalatest.integ.Node
import java.awt.event.MouseEvent
import java.awt.event.KeyEvent

class DefaultTreeActionProvider extends TreeActionProvider {

  protected def open(node: Node) {
    node.getLocation match {
      case Some(location) => 
        location match {
          case topOfClass: TopOfClass => 
            JOptionPane.showMessageDialog(null, "Top of class '" + topOfClass.className + "'.", "Location", JOptionPane.INFORMATION_MESSAGE)
          case topOfMethod: TopOfMethod =>
            JOptionPane.showMessageDialog(null, "Top of method '" + topOfMethod.methodId + "' of class '" + topOfMethod.className + "'.", "Location", JOptionPane.INFORMATION_MESSAGE)
          case lineInFile: LineInFile => 
            JOptionPane.showMessageDialog(null, "Line " + lineInFile.lineNumber + " of source file '" + lineInFile.fileName + "'.", "Location", JOptionPane.INFORMATION_MESSAGE)
          case SeeStackDepthException =>
            node.getStackTraces match {
              case Some(stackTraces) => 
                node.getStackDepth match {
                  case Some(stackDepth) =>
                    val stackTraceElement = stackTraces(stackDepth)
                    if (stackTraceElement.fileName != null && stackTraceElement.lineNumber >= 0)
                      JOptionPane.showMessageDialog(null, "Line " + stackTraceElement.lineNumber + " of source file '" + stackTraceElement.fileName + "'.", "Location", JOptionPane.INFORMATION_MESSAGE)
                    else if (stackTraceElement.className != null && stackTraceElement.methodName != null)
                      JOptionPane.showMessageDialog(null, "At method '" + stackTraceElement.methodName + "' of class '" + stackTraceElement.className + "'.", "Location", JOptionPane.INFORMATION_MESSAGE)
                  case None => 
                    JOptionPane.showMessageDialog(null, "Unable to determine location.", "Location", JOptionPane.INFORMATION_MESSAGE)
                }
              case None =>
                JOptionPane.showMessageDialog(null, "Unable to determine location.", "Location", JOptionPane.INFORMATION_MESSAGE)
            }
        }
      case None =>
        JOptionPane.showMessageDialog(null, "No location provided.", "Location", JOptionPane.INFORMATION_MESSAGE)
    }
  }
  
  override def mousePressed(event: MouseEvent, node: Node) {
    if(node != null && event.getClickCount() == 2) 
      open(node)
  }
  
  override def keyPressed(event: KeyEvent, node: Node) {
    if (event.getKeyCode == KeyEvent.VK_F4) 
      open(node)
  }
}