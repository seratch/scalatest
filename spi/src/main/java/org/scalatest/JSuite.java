package org.scalatest;

public interface JSuite {

  /**
    * Runs this suite of tests.
    *
    * @param testName an optional name of one test to execute. If <code>null</code>, all relevant tests should be executed.
    *                 I.e., <code>null</code> acts like a wildcard that means execute all relevant tests in this <code>Suite</code>.
    * @param args the <code>Args</code> for this run
    *
    * @throws NullPointerException if any passed parameter is <code>null</code>.
    */
  //void runJSuite(String testName, Args args); // Will be testName: nullable String, args: JArgs
  
  /**
   * A <code>Set</code> of test names. If this <code>Suite</code> contains no tests, this method returns an empty <code>Set</code>.
   *
   * <p>
   * Although subclass and subtrait implementations of this method may return a <code>Set</code> whose iterator produces <code>String</code>
   * test names in a well-defined order, the contract of this method does not required a defined order. Subclasses are free to
   * implement this method and return test names in either a defined or undefined order.
   * </p>
   */
  java.util.Set<String> getTestNames();
  
  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names with which tests in this <code>Suite</code> are marked, and
   * whose values are the <code>Set</code> of test names marked with each tag.  If this <code>Suite</code> contains no tags, this
   * method returns an empty <code>Map</code>.
   *
   * <p>
   * Subclasses may implement this method to define and/or discover tags in a custom manner, but overriding method implementations
   * should never return an empty <code>Set</code> as a value. If a tag has no tests, its name should not appear as a key in the
   * returned <code>Map</code>.
   * </p>
   */
  java.util.Map<String, java.util.Set<String>> getTags();
  
  /**
   * The total number of tests that are expected to run when this <code>Suite</code>'s <code>run</code> method is invoked.
   *
   * @param filter a <code>Filter</code> with which to filter tests to count based on their tags
   */
  //int getExpectedTestCount(Filter filter); // Will be JFilter
  
  /**
   * The fully qualified name of the class that can be used to rerun this suite.
   */
  String getRerunner(); // Will return a nullable String
  
  /**
   * This suite's style name.
   *
   * <p>
   * This lifecycle method provides a string that is used to determine whether this suite object's
   * style is one of the <a href="tools/Runner$.html#specifyingChosenStyles">chosen styles</a> for
   * the project.
   * </p>
   */
  String getStyleName(); // Actually, not sure we need this over in the JSuite area Maybe this will just not exist over there.
  // But it might make it simpler to be consistent.
  
  /**
   * A user-friendly suite name for this <code>Suite</code>.
   *
   * <p>
   * This trait's
   * implementation of this method returns the simple name of this object's class. This
   * trait's implementation of <code>runNestedSuites</code> calls this method to obtain a
   * name for <code>Report</code>s to pass to the <code>suiteStarting</code>, <code>suiteCompleted</code>,
   * and <code>suiteAborted</code> methods of the <code>Reporter</code>.
   * </p>
   *
   * @return this <code>Suite</code> object's suite name.
   */
  String getSuiteName();
  
  /**
   * A string ID for this <code>Suite</code> that is intended to be unique among all suites reported during a run.
   *
   * <p>
   * This trait's
   * implementation of this method returns the fully qualified name of this object's class. 
   * Each suite reported during a run will commonly be an instance of a different <code>Suite</code> class,
   * and in such cases, this default implementation of this method will suffice. However, in special cases
   * you may need to override this method to ensure it is unique for each reported suite. For example, if you write
   * a <code>Suite</code> subclass that reads in a file whose name is passed to its constructor and dynamically
   * creates a suite of tests based on the information in that file, you will likely need to override this method
   * in your <code>Suite</code> subclass, perhaps by appending the pathname of the file to the fully qualified class name. 
   * That way if you run a suite of tests based on a directory full of these files, you'll have unique suite IDs for
   * each reported suite.
   * </p>
   *
   * <p>
   * The suite ID is <em>intended</em> to be unique, because ScalaTest does not enforce that it is unique. If it is not
   * unique, then you may not be able to uniquely identify a particular test of a particular suite. This ability is used,
   * for example, to dynamically tag tests as having failed in the previous run when rerunning only failed tests.
   * </p>
   *
   * @return this <code>Suite</code> object's ID.
   */
  String getSuiteId();
}
