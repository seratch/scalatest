package org.scalatest.integ
import javax.swing.JFrame
import javax.swing.WindowConstants
import java.util.Observer
import java.util.Observable

class ResultViewFrame extends JFrame with Observer {
  private val content = getContentPane()
  private val resultView = new ResultView()
  
  setTitle("ScalaTest Run Result")
  setSize(380, 500)
  content.add(resultView)
  
  setVisible(true)
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  
  def update(o: Observable, event: AnyRef) {
    event match {
      case e: Event => resultView.update(o, event)
      case _ => // Ignore others
    }
  }
}