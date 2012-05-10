package org.scalatest.akka

import org.scalatest.concurrent.Eventually
import akka.actor.ActorSystem
import akka.testkit.TestKitExtension

object AkkaEventually extends Eventually {
  override def spanScaleFactor: Double = TestKitExtension.get(ActorSystem()).TestTimeFactor
}