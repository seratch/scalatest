package org.scalatest.integ
import java.io.File
import java.util.Observer
import java.util.Observable

object ScalaTestViewer {

  def main(args: Array[String]) {
    if (args.length < 2)
      throw new IllegalArgumentException("At least 2 arguments expected: classpath + ScalaTest program argument(s).")
    
    val classpath = args(0).split(File.pathSeparator)
    val stArgs = args.tail
    
    val eventPrinter = new Observer() {
      def update(o: Observable, event: AnyRef) {
        event match {
          case e: Event => println(e.getClass.getName)
        }
      }
    }
    
    val runnable = new ScalaTestRunnable(classpath, stArgs)
    runnable.addObserver(eventPrinter)
    val thread = new Thread(runnable)
    thread.start()
  }
  
}