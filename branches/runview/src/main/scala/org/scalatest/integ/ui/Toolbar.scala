package org.scalatest.integ.ui

import javax.swing.JPanel
import java.util.Observer
import java.util.Observable
import java.awt.FlowLayout
import SwingHelper._
import org.scalatest.integ.RunModel
import org.scalatest.integ.RunStatus
import org.scalatest.integ.spi.ToolbarActionProvider
import java.awt.event.ActionListener
import java.awt.event.ActionEvent

class Toolbar(actionProvider: ToolbarActionProvider) extends JPanel with Observer {

  private val nextFailure = createIconButton(Icons.ENABLED_NEXT_FAILURE, Icons.DISABLED_NEXT_FAILURE)
  private val previousFailure = createIconButton(Icons.ENABLED_PREVIOUS_FAILURE, Icons.DISABLED_PREVIOUS_FAILURE)
  
  init()
  
  setLayout(new FlowLayout(FlowLayout.TRAILING))
  add(nextFailure)
  add(previousFailure)
  
  private def init() {
    enableButtons(false)
    nextFailure.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        actionProvider.nextFailure(e)
      }
    })
    previousFailure.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        actionProvider.previousFailure(e)
      }
    })
  }
  
  private def enableButtons(enable: Boolean) {
    nextFailure.setEnabled(enable)
    previousFailure.setEnabled(enable)
  }
  
  def update(o: Observable, value: AnyRef) {
    o match {
      case resultController: ResultController => 
        value match {
          case run: RunModel => 
            run.status match {
              case RunStatus.STARTED => 
                invokeLater { enableButtons(false) }
              case RunStatus.COMPLETED =>
                
              case RunStatus.STOPPED =>
                
              case RunStatus.ABORTED => 
                
            }
          case treeSelectedEvent: TreeSelectedEvent =>
            nextFailure.setEnabled(treeSelectedEvent.hasNextFailure)
            previousFailure.setEnabled(treeSelectedEvent.hasPreviousFailure)
          case _ => 
            // Ignore others
        }
      case _ => 
        // Do nothing if the observable is not ResultModel
    }
  }
  
}