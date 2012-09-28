package org.scalatest;

public interface JReporter {
    
  JFormatter createMotionToSuppress();
  
  JFormatter createIndentedText(String formattedText, String rawText, int indentationLevel);

  JRecordableEvent createInfoProvided(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, String testName, 
                                      Throwable throwable, JFormatter formatter, JLocation location, Object payload);
  
  JRecordableEvent createMarkupProvided(JOrdinal ordinal, String text, String suiteName, String suiteId, String suiteClassName, String testName,
                                        JFormatter formatter, JLocation location, Object payload);
  
  JLocation createTopOfClass(String className);
  
  JLocation createTopOfMethod(String className, String methodId);
  
  JLocation createLineInFile(int lineNumber, String fileName);
  
  JLocation createSeeStackDepthException();
  
  void fireTestStarting(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JFormatter formatter,
                         JLocation location, String rerunner, Object payload);
  
  void fireTestSucceeded(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JRecordableEvent[] recordedEvents, 
                         long duration, JFormatter formatter, JLocation location, String rerunner, Object payload);
  
  void fireTestFailed(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, String testName, String testText, 
                      JRecordableEvent[] recordedEvents, Throwable throwable, long duration, JFormatter formatter, JLocation location, String rerunner, Object payload);
  
  void fireTestIgnored(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JFormatter formatter, JLocation location,
                       Object payload);
  
  void fireTestPending(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JRecordableEvent[] recordedEvents, 
                        long duration, JFormatter formatter, JLocation location, Object payload);
  
  void fireTestCanceled(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JRecordableEvent[] recordedEvents, 
                        Throwable throwable, long duration, JFormatter formatter, JLocation location, Object payload);
  
  void fireScopeOpened(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, JFormatter formatter, JLocation location, Object payload);
  
  void fireScopeClosed(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, JFormatter formatter, JLocation location, Object payload);
  
  void fireSuiteStarting(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, JFormatter formatter, JLocation location, String rerunner, Object payload);
  
  void fireSuiteCompleted(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, long duration, JFormatter formatter, JLocation location, String rerunner,
                          Object payload);
  
  void fireSuiteAborted(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, Throwable throwable, long duration, JFormatter formatter,
                        JLocation location, String rerunner, Object payload);
  
  void fireInfoProvided(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, String testName, 
                        Throwable throwable, JFormatter formatter, JLocation location, Object payload);
  
  void fireMarkupProvided(JOrdinal ordinal, String text, String suiteName, String suiteId, String suiteClassName, String testName, JFormatter formatter, JLocation location, Object payload);
}
