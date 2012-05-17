package org.scalatest.integ
import java.util.Observer
import java.util.Observable
import java.io.File
import java.io.BufferedReader
import java.io.InputStreamReader

class ScalaTestRunner(classPath: Array[String], stArgs: Array[String], observer: Observer) {
  
  def run() {
    // Run ScalaTest
    val runnable = new ScalaTestRunnable(classPath, stArgs)
    runnable.addObserver(observer)
    val thread = new Thread(runnable)
    thread.start()
  }
  
  def runFailed(failedTestList: List[TestModel]) {
    val failedTestArgs = failedTestList.map { test => getScalaTestArgsForTest(test.suiteClassName.getOrElse(null), test.suiteId, test.testName) }.flatten
    val runnable = new ScalaTestRunnable(classPath, failedTestArgs.toArray)
    runnable.addObserver(observer)
    val thread = new Thread(runnable)
    thread.start()
  }
  
  private def getScalaTestArgsForTest(suiteClassName: String, suiteId: String, testName: String) = {
    if (suiteClassName == suiteId)
      List("-s", suiteClassName, "-t", testName)
    else
      List("-s", suiteClassName, "-i", suiteId, "-t", testName)
  }
}

private class ScalaTestRunnable(classPath: Array[String], stArgs: Array[String]) extends Observable with Observer with Runnable {

  private val listener: EventListener = new EventListener(0)
  listener.addObserver(this)
  
  def getListenerPort = listener.getPort
  
  def run() {
    val listenerThread = new Thread(listener)
    listenerThread.start()
    val args = "java" :: "-cp" :: classPath.mkString(File.pathSeparator) :: "org.scalatest.tools.Runner" :: stArgs.toList ::: "-k" :: "localhost" :: listener.getPort.toString() :: Nil
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