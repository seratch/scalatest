package org.scalatest.integ
import java.io.File
import java.util.Observer
import java.util.Observable
import javax.swing.UIManager
import org.scalatest.integ.ui.ResultViewFrame
import org.scalatest.integ.ui.RerunAllEvent

object ScalaTestViewer extends Observer {

  private var runner: ScalaTestRunner = null
  
  def main(args: Array[String]) {
    if (args.length < 2)
      throw new IllegalArgumentException("At least 2 arguments expected: classpath + ScalaTest program argument(s).")
    
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    val viewFrame = new ResultViewFrame()
    viewFrame.addObserver(this)
    
    val classpath = args(0).split(File.pathSeparator)
    val stArgs = args.tail
    
    runner = new ScalaTestRunner(classpath, stArgs, viewFrame)
    runner.run()
  }
  
  def update(o: Observable, event: AnyRef) {
    event match {
      case rerunAll: RerunAllEvent => 
        runner.run()
      case _ =>
        // Ignore others
    }
  }
  
  private class ScalaTestRunner(classPath: Array[String], stArgs: Array[String], viewFrame: ResultViewFrame) {
    def run() {
      // Run ScalaTest
      val runnable = new ScalaTestRunnable(classPath, stArgs)
      runnable.addObserver(viewFrame)
      val thread = new Thread(runnable)
      thread.start()
    }
  }
}