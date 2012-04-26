package org.scalatest.integ
import java.util.Observer
import java.util.Observable
import java.io.File
import java.io.BufferedReader
import java.io.InputStreamReader

class ScalaTestRunnable(classPath: Array[String], stArgs: Array[String]) extends Observable with Observer with Runnable {

  private val listener: EventListener = new EventListener(0)
  listener.addObserver(this)
  
  def getListenerPort = listener.getPort
  
  def run() {
    val listenerThread = new Thread(listener)
    listenerThread.start()
    val args = "java" :: "-cp" :: classPath.mkString(File.pathSeparator) :: "org.scalatest.tools.Runner" :: stArgs.toList ::: "-k" :: "localhost" :: listener.getPort.toString() :: Nil
    println(args)
    val builder = new ProcessBuilder(args: _*)
    val process = builder.start()
    
    // Print process output
    val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
    var line = reader.readLine  
    while (line != null) {
      println ("Stdout: " + line)
      line = reader.readLine
    }
  }
  
  def send(event: AnyRef){
    setChanged()
    notifyObservers(event)
  }
  
  def update(o: Observable, event: AnyRef) {
    send(event)
    event match {
      case e: RunCompleted => listener.stop()
      case e: RunStopped => listener.stop()
      case e: RunAborted => listener.stop()
      case _ =>
    }
  }
}