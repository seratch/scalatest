package org.scalatest

import Filter.IgnoreTag

/*
dynamicTags: Option[(suiteID: String, testName: String) -> Set[String]]

Then to apply, I need to add the suiteID. So backwards compatibility is a question.
If I don't have a suiteID, then I can't really... Well if I don't have a suiteID, 
could I simply ignore the dynamic tags. That could be deprecated behavior.
Probably good enough.

A filter can contain a set of dynamic tags. So that's the first thing. Provide a method on filter
that returns the same filter, but with dynamic tags added?
*/

/**
 * Filter whose <code>apply</code> method determines which of the passed tests to run and ignore based on tags to include and exclude passed as
 * as class parameters.
 *
 * <p>
 * The behavior of <code>Filter</code> can be broken down into four cases:
 * </p>
 *
 * <p>
 * 1. If both <code>tagsToInclude</code> and <code>testNamesToInclude</code> are <code>None</code>, then only test names tagged with one or more tags in
 * the <code>tagsToExclude</code> set will be filtered out by <code>Filter</code>'s <code>apply</code> methods.
 * </p>
 *
 * <p>
 * 2. If <code>tagsToInclude</code> is 
 * defined, but <code>testNamesToInclude</code> is not, then test names without any tags mentioned in <code>tagsToInclude</code> as well as
 * with a tag mentioned in <code>tagsToExclude</code> will be filtered out. (A test with tags mentioned in both <code>tagsToInclude</code> and <code>tagsToExclude</code> will
 * be filtered out.)
 * </p>
 *
 * <p>
 * 3. If <code>testNamesToInclude</code> is defined, but <code>tagsToInclude</code> is not, then test names not mentioned in
 * <code>testNamesToInclude</code> as well as any test name with one or more tags mentioned in <code>tagsToExclude</code> will be filtered out. (A test with a name
 * mentioned in <code>testNamesToInclude</code> and one or more tags mentioned in <code>tagsToExclude</code> will be filtered out.)
 * </p>
 *
 * <p>
 * 4. If both <code>tagsToInclude</code> and <code>testNamesToInclude</code> are defined, then the result of <code>apply</code> will essentially be the
 * intersection of cases 2 and 3. To survive the filter, a test's name must both be included in <code>testNamesToInclude</code> and have at least one
 * tag mentioned in <code>tagsToInclude</code>.
 * </p>
 *
 * <p>
 * This class handles the <code>org.scalatest.Ignore</code> tag specially, in that its <code>apply</code> method indicates which
 * tests should be ignored based on whether they are tagged with <code>org.scalatest.Ignore</code>. If
 * <code>"org.scalatest.Ignore"</code> is not passed in the <code>tagsToExclude</code> set, it will be implicitly added. However, if the 
 * <code>tagsToInclude</code> option is defined, and the contained set does not include <code>"org.scalatest.Ignore"</code>, then only those tests
 * that are both tagged with <code>org.scalatest.Ignore</code> and at least one of the tags in the <code>tagsToInclude</code> set
 * will be included in the result of <code>apply</code> and marked as ignored (so long as the test is not also
 * marked with a tag other than <code>org.scalatest.Ignore</code> that is a member of the <code>tagsToExclude</code>
 * set. For example, if <code>SlowAsMolasses</code> is a member of the <code>tagsToInclude</code> set and a
 * test is tagged with both <code>org.scalatest.Ignore</code> and <code>SlowAsMolasses</code>, and
 * <code>SlowAsMolasses</code> appears in the <code>tagsToExclude</code> set, the
 * <code>SlowAsMolasses</code> tag will "overpower" the <code>org.scalatest.Ignore</code> tag, and the
 * test will be filtered out entirely rather than being ignored.
 * </p>
 *
 * @param tagsToInclude an optional <code>Set</code> of <code>String</code> tag names to include (<em>i.e.</em>, not filter out) when filtering tests
 * @param tagsToExclude a <code>Set</code> of <code>String</code> tag names to exclude (<em>i.e.</em>, filter out) when filtering tests
 * @param testNamesToInclude an optional <code>Set</code> of <code>String</code> test names to include (<em>i.e.</em>, not filter out) when filtering tests
 *
 * @throws NullPointerException if either <code>tagsToInclude</code> or <code>tagsToExclude</code> are null
 * @throws IllegalArgumentException if <code>tagsToInclude</code> is defined, but contains an empty set, or if <code>testNamesToInclude</code> is defined, but contains an empty set
 */
final class Filter(val tagsToInclude: Option[Set[String]], val tagsToExclude: Set[String], val testNamesToInclude: Option[Set[String]] = None) extends Function2[Set[String], Map[String, Set[String]], List[(String, Boolean)]] {

  if (tagsToInclude == null)
    throw new NullPointerException("tagsToInclude was null")
  if (tagsToExclude == null)
    throw new NullPointerException("tagsToExclude was null")
  if (testNamesToInclude == null)
    throw new NullPointerException("testNamesToInclude was null")

  tagsToInclude match {
    case Some(tagsToInclude) =>
      if (tagsToInclude.isEmpty)
        throw new IllegalArgumentException("tagsToInclude was defined, but contained an empty set")
    case None =>
  }

  testNamesToInclude match {
    case Some(testNamesToInclude) =>
      if (testNamesToInclude.isEmpty)
        throw new IllegalArgumentException("testNamesToInclude was defined, but contained an empty set")
    case None =>
  }

  private def includedTestNames(testNamesAsList: List[String], tags: Map[String, Set[String]]): List[String] = {
    val testNamesToIncludeBasedJustOnTags =
      tagsToInclude match {
        case None => testNamesAsList
        case Some(tagsToInclude) =>
          for {
            testName <- testNamesAsList
            if tags contains testName
            intersection = tagsToInclude intersect tags(testName)
            if intersection.size > 0
          } yield testName
      }
    testNamesToInclude match {
      case None => testNamesToIncludeBasedJustOnTags 
      case Some(tnToInclude) =>
        testNamesToIncludeBasedJustOnTags intersect tnToInclude.toList
    }
  }

  private def verifyPreconditionsForMethods(testNames: Set[String], tags: Map[String, Set[String]]) {
    val testWithEmptyTagSet = tags.find(tuple => tuple._2.isEmpty)
    testWithEmptyTagSet match {
      case Some((testName, _)) => throw new IllegalArgumentException(testName + " was associated with an empty set in the map passsed as tags")
      case None =>
    }
  }

// TODO: Need to update these descriptions of the apply methods, etc., to include the behavior if testNamesToInclude is defined.
  /**
   * Filter test names based on their tags.
   *
   * <p>
   * Each tuple in the returned list contains a <code>String</code>
   * test name and a <code>Boolean</code> that indicates whether the test should be ignored. A test will be marked as ignored
   * if <code>org.scalatest.Ignore</code> is in its tags set, and either <code>tagsToInclude</code> is <code>None</code>, or
   * <code>tagsToInclude</code>'s value (a set) contains the test's name, unless another tag for that test besides <code>org.scalatest.Ignore</code>
   * is also included in <code>tagsToExclude</code>. For example, if a test is tagged with
   * both <code>org.scalatest.Ignore</code> and <code>SlowAsMolasses</code>, and <code>SlowAsMolasses</code>
   * appears in the <code>tagsToExclude</code> set, the <code>SlowAsMolasses</code> tag will
   * "overpower" the <code>org.scalatest.Ignore</code> tag, and this method will return
   * a list that does not include the test name.
   * </p>
   *
   * <pre class="stHighlight">
   * for ((testName, ignoreTest) <- filter(testNames, tags))
   *   if (ignoreTest)
   *     // ignore the test
   *   else
   *     // execute the test
   * </pre>
   *
   * @param testNames test names to be filtered
   * @param tags a map from test name to tags, containing only test names included in the <code>testNames</code> set, and
   *   only test names that have at least one tag
   *
   * @throws IllegalArgumentException if any set contained in the passed <code>tags</code> map is empty
   */
  def apply(testNames: Set[String], tags: Map[String, Set[String]]): List[(String, Boolean)] = {

    verifyPreconditionsForMethods(testNames, tags)

    val testNamesAsList = testNames.toList // to preserve the order
    val filtered =
      for {
        testName <- includedTestNames(testNamesAsList, tags)
        if !tags.contains(testName) ||
                (tags(testName).contains(IgnoreTag) && (tags(testName) intersect (tagsToExclude + "org.scalatest.Ignore")).size == 1) ||
                (tags(testName) intersect tagsToExclude).size == 0
      } yield (testName, tags.contains(testName) && tags(testName).contains(IgnoreTag))

    filtered
  }

  /**
   * Filter one test name based on its tags.
   *
   * <p>
   * The returned tuple contains a <code>Boolean</code>
   * that indicates whether the test should be filtered, and if not, a <code>Boolean</code> that
   * indicates whether the test should be ignored. A test will be marked as ignored
   * if <code>org.scalatest.Ignore</code> is in its tags set, and either <code>tagsToInclude</code>
   * is <code>None</code>, or <code>tagsToInclude</code>'s value (a set) contains the passed
   * test name, unless another tag for that test besides <code>org.scalatest.Ignore</code>
   * is also included in <code>tagsToExclude</code>. For example, if a test is tagged with
   * both <code>org.scalatest.Ignore</code> and <code>SlowAsMolasses</code>, and <code>SlowAsMolasses</code>
   * appears in the <code>tagsToExclude</code> set, the <code>SlowAsMolasses</code> tag will
   * "overpower" the <code>org.scalatest.Ignore</code> tag, and this method will return
   * (true, false). 
   * </p>
   * 
   * <pre class="stHighlight">
   * val (filterTest, ignoreTest) = filter(testName, tags)
   * if (!filterTest)
   *   if (ignoreTest)
   *     // ignore the test
   *   else
   *     // execute the test
   * </pre>
   *
   * @param testName the test name to be filtered
   * @param tags a map from test name to tags, containing only test names that have at least one tag
   *
   * @throws IllegalArgumentException if any set contained in the passed <code>tags</code> map is empty
   */
  def apply(testName: String, tags: Map[String, Set[String]]): (Boolean, Boolean) = {
    val list = apply(Set(testName), tags)
    if (list.isEmpty)
      (true, false)
    else
      (false, list.head._2)
  }

  /**
   * Returns the number of tests that should be run after the passed <code>testNames</code> and <code>tags</code> have been filtered
   * with the <code>tagsToInclude</code> and <code>tagsToExclude</code> class parameters.
   *
   * <p>
   * The result of this method may be smaller than the number of
   * elements in the list returned by <code>apply</code>, because the count returned by this method does not include ignored tests,
   * and the list returned by <code>apply</code> does include ignored tests.
   * </p>
   *
   * @param testNames test names to be filtered
   * @param tags a map from test name to tags, containing only test names included in the <code>testNames</code> set, and
   *   only test names that have at least one tag
   *
   * @throws IllegalArgumentException if any set contained in the passed <code>tags</code> map is empty
   */
  def runnableTestCount(testNames: Set[String], tags: Map[String, Set[String]]): Int = {

    verifyPreconditionsForMethods(testNames, tags)

    val testNamesAsList = testNames.toList // to preserve the order
    val runnableTests = 
      for {
        testName <- includedTestNames(testNamesAsList, tags)
        if !tags.contains(testName) || (!tags(testName).contains(IgnoreTag) && (tags(testName) intersect tagsToExclude).size == 0)
      } yield testName

    runnableTests.size
  }
}

object Filter {
  private final val IgnoreTag = "org.scalatest.Ignore"

/**
 * Factory method for a <code>Filter</code> initialized with the passed <code>tagsToInclude</code>
 * and <code>tagsToExclude</code>.
 *
 * @param tagsToInclude an optional <code>Set</code> of <code>String</code> tag names to include (<em>i.e.</em>, not filter out) when filtering tests
 * @param tagsToExclude a <code>Set</code> of <code>String</code> tag names to exclude (<em>i.e.</em>, filter out) when filtering tests
 *
 * @throws NullPointerException if either <code>tagsToInclude</code> or <code>tagsToExclude</code> are null
 * @throws IllegalArgumentException if <code>tagsToInclude</code> is defined, but contains an empty set
 */
  def apply(tagsToInclude: Option[Set[String]], tagsToExclude: Set[String]) =
    new Filter(tagsToInclude, tagsToExclude)

/**
 * Factory method for a <code>Filter</code> initialized with <code>None</code> for <code>tagsToInclude</code>
 * and an empty set for <code>tagsToExclude</code>.
 *
 * @param tagsToInclude an optional <code>Set</code> of <code>String</code> tag names to include (<em>i.e.</em>, not filter out) when filtering tests
 * @param tagsToExclude a <code>Set</code> of <code>String</code> tag names to exclude (<em>i.e.</em>, filter out) when filtering tests
 *
 * @throws NullPointerException if either <code>tagsToInclude</code> or <code>tagsToExclude</code> are null
 * @throws IllegalArgumentException if <code>tagsToInclude</code> is defined, but contains an empty set
 */
  def apply() =
    new Filter(None, Set("org.scalatest.Ignore"))
}