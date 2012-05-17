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
import java.awt.event.ItemListener
import java.awt.event.ItemEvent

class Toolbar(actionProvider: ToolbarActionProvider) extends JPanel with Observer {

  private val nextFailure = createIconButton(Icons.ENABLED_NEXT_FAILURE, Icons.DISABLED_NEXT_FAILURE)
  private val previousFailure = createIconButton(Icons.ENABLED_PREVIOUS_FAILURE, Icons.DISABLED_PREVIOUS_FAILURE)
  private val showFailureOnly = createIconToggleButton(Icons.SHOW_FAILURE_ONLY, Icons.SHOW_FAILURE_ONLY)
  private val rerunAll = createIconButton(Icons.ENABLED_RERUN_ALL, Icons.DISABLED_RERUN_ALL)
  private val rerunFailed = createIconButton(Icons.ENABLED_RERUN_FAILED, Icons.DISABLED_RERUN_FAILED)
  
  init()
  
  setLayout(new FlowLayout(FlowLayout.TRAILING))
  add(nextFailure)
  add(previousFailure)
  add(showFailureOnly)
  add(rerunAll)
  add(rerunFailed)
  
  private def init() {
    disableButtons()
    nextFailure.setToolTipText("Next Failed Test")
    nextFailure.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        actionProvider.nextFailure(e)
      }
    })
    previousFailure.setToolTipText("Previous Failed Test")
    previousFailure.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        actionProvider.previousFailure(e)
      }
    })
    showFailureOnly.setToolTipText("Show Failed Tests Only")
    showFailureOnly.addItemListener(new ItemListener() {
      def itemStateChanged(e: ItemEvent) {
        val state = e.getStateChange
        actionProvider.showFailureOnly(e, state == ItemEvent.SELECTED)
      }
    })
    rerunAll.setToolTipText("Rerun All Tests")
    rerunAll.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        actionProvider.rerunAll(e)
      }
    })
    rerunFailed.setToolTipText("Rerun Failed Tests")
    rerunFailed.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        actionProvider.rerunFailed(e)
      }
    })
  }
  
  private def disableButtons() {
    nextFailure.setEnabled(false)
    previousFailure.setEnabled(false)
    rerunAll.setEnabled(false)
    rerunFailed.setEnabled(false)
  }
  
  private def enableButtons() {
    rerunAll.setEnabled(true)
    rerunFailed.setEnabled(true)
  }
  
  def update(o: Observable, value: AnyRef) {
    o match {
      case resultController: ResultController => 
        value match {
          case run: RunModel => 
            run.status match {
              case RunStatus.STARTED => 
                invokeLater { disableButtons() }
              case RunStatus.COMPLETED =>
                invokeLater { enableButtons() }
              case RunStatus.STOPPED =>
                invokeLater { enableButtons() }
              case RunStatus.ABORTED => 
                invokeLater { enableButtons() }
            }
          case treeSelectedEvent: TreeSelectedEvent =>
            nextFailure.setEnabled(treeSelectedEvent.hasNextFailure)
            previousFailure.setEnabled(treeSelectedEvent.hasPreviousFailure)
          case _ => 
            // Ignore others
        }
      case _ => 
        // Do nothing if the observable is not ResultController
    }
  }
  
}