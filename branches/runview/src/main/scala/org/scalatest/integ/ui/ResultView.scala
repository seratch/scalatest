package org.scalatest.integ.ui

import javax.swing.JPanel
import java.util.Observer
import java.awt.BorderLayout
import java.util.Observable
import org.scalatest.integ.Event
import org.scalatest.integ.spi.impl.DefaultTreeActionProvider

class ResultView extends JPanel with Observer {
  
  private val counterPanel = new CounterPanel()
  private val colorBar = new ColorBar()
  private val resultTree = new ResultTree(treeActionProvider)
  
  private val resultController = new ResultController()
  resultController.addObserver(counterPanel)
  resultController.addObserver(colorBar)
  resultController.addObserver(resultTree)
  
  private val controlPanel = new JPanel() {
    setLayout(new BorderLayout())
    add(counterPanel, BorderLayout.CENTER)
    add(colorBar, BorderLayout.SOUTH)
  }
  
  setLayout(new BorderLayout())
  add(controlPanel, BorderLayout.NORTH)
  add(resultTree, BorderLayout.CENTER)
  
  def update(o: Observable, event: AnyRef) {
    event match {
      case e: Event => resultController.update(e)
      case _ => // Ignore others
    }
  }
  
  def treeActionProvider = new DefaultTreeActionProvider()
}

/**/