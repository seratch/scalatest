package org.scalatest;

public interface JReporter {
    
  JFormatter createMotionToSuppress();
  
  JFormatter createIndentedText(String formattedText, String rawText, int indentationLevel);

  JRecordableEvent createInfoProvided(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, String testName, 
                                      Throwable throwable, JFormatter formatter, JLocation location, Object payload, String threadName, long timeStamp);
  
  JRecordableEvent createMarkupProvided(JOrdinal ordinal, String text, String suiteName, String suiteId, String suiteClassName, String testName,
                                        JFormatter formatter, JLocation location, Object payload, String threadName, long timeStamp);
  
  JLocation createTopOfClass(String className);
  
  JLocation createTopOfMethod(String className, String methodId);
  
  JLocation createLineInFile(int lineNumber, String fileName);
  
  JLocation createSeeStackDepthException();
  
  void fireTestStarting(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JFormatter formatter,
                         JLocation location, String rerunner, Object payload, String threadName, long timeStamp);
  
  void fireTestSucceeded(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JRecordableEvent[] recordedEvents, 
                         long duration, JFormatter formatter, JLocation location, String rerunner, Object payload, String threadName, long timeStamp);
  
  void fireTestFailed(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, String testName, String testText, 
                      JRecordableEvent[] recordedEvents, Throwable throwable, long duration, JFormatter formatter, JLocation location, String rerunner, Object payload,
                      String threadName, long timeStamp);
  
  void fireTestIgnored(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JFormatter formatter, JLocation location,
                       Object payload, String threadName, long timeStamp);
  
  void fireTestPending(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JRecordableEvent[] recordedEvents, 
                        long duration, JFormatter formatter, JLocation location, Object payload, String threadName, long timeStamp);
  
  void fireTestCanceled(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, String testName, String testText, JRecordableEvent[] recordedEvents, 
                        Throwable throwable, long duration, JFormatter formatter, JLocation location, Object payload, String threadName, long timeStamp);
  
  void fireSuiteStarting(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, JFormatter formatter, JLocation location, String rerunner, Object payload,
                         String threadName, long timeStamp);
  
  void fireSuiteCompleted(JOrdinal ordinal, String suiteName, String suiteId, String suiteClassName, long duration, JFormatter formatter, JLocation location, String rerunner,
                          Object payload, String threadName, long timeStamp);
  
  void fireSuiteAborted(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, Throwable throwable, long duration, JFormatter formatter,
                        JLocation location, String rerunner, Object payload, String threadName, long timeStamp);
  
  void fireInfoProvided(JOrdinal ordinal, String message, String suiteName, String suiteId, String suiteClassName, String testName, 
                        Throwable throwable, JFormatter formatter, JLocation location, Object payload, String threadName, long timeStamp);
  
  void fireMarkupProvided(JOrdinal ordinal, String text, String suiteName, String suiteId, String suiteClassName, String testName, JFormatter formatter, JLocation location, Object payload, 
                          String threadName, long timeStamp);
}
