package org.scalatest.integ.ui

import javax.swing.JPanel
import java.util.Observer
import java.awt.BorderLayout
import javax.swing.JList
import javax.swing.JLabel
import javax.swing.SwingConstants
import javax.swing.JToggleButton
import java.util.Observable
import org.scalatest.integ.Node
import javax.swing.DefaultListModel
import javax.swing.JScrollPane
import org.scalatest.integ.spi.StackTraceActionProvider
import java.awt.event.ItemListener
import java.awt.event.ItemEvent

class StackTraceView(actionProvider: StackTraceActionProvider) extends JPanel with Observer {

  setLayout(new BorderLayout())
  
  private val headerPanel = new JPanel()
  private val headerLabel = new JLabel("Stack Trace", Icons.STACKTRACE, SwingConstants.LEFT)
  private val stackFoldButton = new JToggleButton(Icons.STACKFOLD, true)
  private val stackTraceList = new JList()
  private val stackTraceModel = new DefaultListModel()
  
  private var selectedNode: Node = null
  
  init()
  
  private def init() {
    stackFoldButton.setToolTipText("Fold Stack Trace")
    stackFoldButton.addItemListener(new ItemListener() {
      def itemStateChanged(e: ItemEvent) {
        refresh()
      }
    })
    
    headerPanel.setLayout(new BorderLayout())
    headerPanel.add(headerLabel, BorderLayout.WEST)
    headerPanel.add(stackFoldButton, BorderLayout.EAST)
    
    stackTraceList.setModel(stackTraceModel)
    
    add(headerPanel, BorderLayout.NORTH)
    add(new JScrollPane(stackTraceList), BorderLayout.CENTER)
  }
  
  private def refresh() {
    stackTraceModel.clear()
    if (selectedNode != null) {
      selectedNode.getStackTraces match {
        case Some(stackTraces) => 
          val filtered = 
            if (stackFoldButton.isSelected) {
              selectedNode.getStackDepth match {
                case Some(stackDepth) =>
                  stackTraces.drop(stackDepth)
                case None =>
                  stackTraces
              }
            }
            else
              stackTraces    
          filtered.foreach {st => stackTraceModel.addElement(actionProvider.stackTraceString(st)) }
        case None =>
                    
      }
    }
  }
  
  def update(o: Observable, value: AnyRef) {
    o match {
      case resultController: ResultController => 
        value match {
          case treeSelectedEvent: TreeSelectedEvent =>
            selectedNode = treeSelectedEvent.selected
            refresh()
          case _ => 
            // Ignore others
        }
      case _ => 
        // Do nothing if the observable is not ResultController
    }
  }
}