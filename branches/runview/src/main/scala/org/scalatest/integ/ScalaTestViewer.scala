package org.scalatest.integ
import java.io.File
import java.util.Observer
import java.util.Observable
import javax.swing.UIManager
import org.scalatest.integ.ui.ResultViewFrame

object ScalaTestViewer {

  def main(args: Array[String]) {
    if (args.length < 2)
      throw new IllegalArgumentException("At least 2 arguments expected: classpath + ScalaTest program argument(s).")
    
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    val viewFrame = new ResultViewFrame()
    
    val classpath = args(0).split(File.pathSeparator)
    val stArgs = args.tail
    
    // Run ScalaTest
    val runnable = new ScalaTestRunnable(classpath, stArgs)
    runnable.addObserver(viewFrame)
    val thread = new Thread(runnable)
    thread.start()
  }
  
}