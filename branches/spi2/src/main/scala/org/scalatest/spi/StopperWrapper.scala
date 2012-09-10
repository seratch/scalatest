package org.scalatest.spi
import org.scalatest.{JStopper, Stopper}

class StopperWrapper(stopper: Stopper) extends JStopper {

  def isStopRequested = stopper.apply()
  
}