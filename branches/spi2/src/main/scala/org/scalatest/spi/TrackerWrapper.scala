package org.scalatest.spi
import org.scalatest.{JTracker, Tracker}

class TrackerWrapper(tracker: Tracker) extends JTracker {

  def nextOrdinal = tracker.nextOrdinal
  
  def nextTracker = new TrackerWrapper(tracker)
  
}