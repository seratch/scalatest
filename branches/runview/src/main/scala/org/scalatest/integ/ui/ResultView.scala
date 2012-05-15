package org.scalatest.integ.ui

import javax.swing.JPanel
import java.util.Observer
import java.awt.BorderLayout
import java.util.Observable
import org.scalatest.integ.Event
import org.scalatest.integ.spi.impl.DefaultTreeActionProvider
import org.scalatest.integ.spi.impl.DefaultToolbarActionProvider
import java.awt.GridLayout
import org.scalatest.integ.spi.impl.DefaultStackTraceActionProvider

class ResultView extends JPanel with Observer {
  
  private val resultController = new ResultController()
  
  private val toolbar = new Toolbar(toolbarActionProvider(resultController))
  private val counterPanel = new CounterPanel()
  private val colorBar = new ColorBar()
  private val gridPanel = new JPanel()
  private val resultTree = new ResultTree(treeActionProvider(resultController))
  private val stackTrace = new StackTraceView(stackTraceActionProvider(resultController))
  
  resultController.addObserver(toolbar)
  resultController.addObserver(counterPanel)
  resultController.addObserver(colorBar)
  resultController.addObserver(resultTree)
  resultController.addObserver(stackTrace)
  
  private val controlPanel = new JPanel() {
    setLayout(new BorderLayout())
    add(toolbar, BorderLayout.NORTH)
    add(counterPanel, BorderLayout.CENTER)
    add(colorBar, BorderLayout.SOUTH)
  }
  
  setLayout(new BorderLayout())
  add(controlPanel, BorderLayout.NORTH)
  gridPanel.setLayout(new GridLayout(2, 1))
  gridPanel.add(resultTree)
  gridPanel.add(stackTrace)
  add(gridPanel, BorderLayout.CENTER)
  
  def update(o: Observable, event: AnyRef) {
    event match {
      case e: Event => resultController.update(e)
      case _ => // Ignore others
    }
  }
  
  def notifyChanges(value: AnyRef) {
    resultController.notifyChanges(value)
  }
  
  def toolbarActionProvider(controller: ResultController) = new DefaultToolbarActionProvider(controller)
  def treeActionProvider(controller: ResultController) = new DefaultTreeActionProvider(controller)
  def stackTraceActionProvider(controller: ResultController) = new DefaultStackTraceActionProvider(controller)
}