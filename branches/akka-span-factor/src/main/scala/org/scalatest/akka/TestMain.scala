package org.scalatest.akka
import akka.testkit.TestKitExtension
import akka.actor.ActorSystem

object TestMain {

  def main(args: Array[String]): Unit = {
    println("TestTimeFactor: " + TestKitExtension.get(ActorSystem()).TestTimeFactor)
  }

}