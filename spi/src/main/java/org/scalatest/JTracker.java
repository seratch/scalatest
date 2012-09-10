package org.scalatest;

public interface JTracker {
    
  JOrdinal nextOrdinal();
  
  JTracker nextTracker();
  
}