package org.scalatest;

import java.util.Map;
import java.util.Set;

public interface JArgs {
  
  JReporter reporter();
  
  JStopper stopper();
  
  JFilter filter();
  
  Map<String, Object> configMap();
  
  JTracker tracker();
  
  Set<String> chosenStyles();
  
}
