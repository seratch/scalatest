package org.scalatest;

import java.util.Map;
import java.util.Set;

public interface JFilter {
    
  Map<String, Boolean> doFilter(Set<String> testNames, Map<String, Set<String>> tags, String suiteId);
    
  Boolean[] doFilter(String testName, Map<String, Set<String>> tags, String suiteId);
  
  int runnableTestCount(Set<String> testNames, Map<String, Set<String>> testTags, String suiteId);
}