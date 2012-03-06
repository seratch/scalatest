package org.scalatest

import scala.collection.immutable.TreeSet

class FilterSpec extends FunSpec {
  
  val suite = new Suite() { override def suiteId = "testSuiteId" }
  
  describe("A Filter") {

    it("should throw NPEs if constructed with nulls") {
      intercept[NullPointerException] {
        new Filter(null, null)
      }
      intercept[NullPointerException] {
        new Filter(None, null)
      }
      intercept[NullPointerException] {
        new Filter(null, Set())
      }
      intercept[NullPointerException] {
        new Filter (None, Set(), true, null)
      }
      intercept[NullPointerException] {
        new Filter(None, Set(), true, DynaTags(null, null))
      }
      intercept[NullPointerException] {
        new Filter(None, Set(), true, DynaTags(Map.empty, null))
      }
      intercept[NullPointerException] {
        new Filter(None, Set(), true, DynaTags(null, Map.empty))
      }
    }

    it("should throw IAE if passed a Some(Set()) for tagsToInclude") {
      intercept[IllegalArgumentException] {
        new Filter(Some(Set()), Set())
      }
    }

    it("should throw IAE if passed an empty set for testName in the apply method") {
      val caught = intercept[IllegalArgumentException] {
        val filter = new Filter(None, Set())
        filter(Set("hi", "ho"), Map("hi" -> Set[String]()), suite)
      }
      assert(caught.getMessage === "hi was associated with an empty set in the map passsed as tags")
    }

    it("should throw IAE if passed an empty set for testName in the includedTestCount method") {
      val caught = intercept[IllegalArgumentException] {
        val filter = new Filter(None, Set())
        filter.runnableTestCount(Set("hi", "ho"), Map("hi" -> Set()), suite)
      }
      assert(caught.getMessage === "hi was associated with an empty set in the map passsed as tags")
    }

    val potentialTestNames = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
    val potentialTagNames = List("tag0", "tag1", "tag2", "tag3", "org.scalatest.Ignore", "tag5", "tag6", "tag7", "tag9", "tag9")

    def randomPositiveInt(max: Int) = (Math.random * 10000).toInt % (max + 1)

    def validateIgnoreBehavior(filter: Filter) {
      val filtered = filter(Set("myTestName"), Map("myTestName" -> Set("org.scalatest.Ignore")), suite)
      assert(filtered exists (tuple => tuple._1 == "myTestName"), "myTestName was in the tags map, but did not show up in the result of apply") 
      assert(filtered exists (tuple => tuple._1 == "myTestName" && tuple._2 == true), "myTestName was in the result of apply, but was not marked as ignored") 
    }

    it("should report a test as ignored when None is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(None, Set("no ignore here"))
      validateIgnoreBehavior(filter)
    }

    it("should report a test as ignored when None is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(None, Set("org.scalatest.Ignore"))
      validateIgnoreBehavior(filter)
    }

    it("should report a test as ignored when Some(Ignore) is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.Ignore")), Set("no ignore here"))
      validateIgnoreBehavior(filter)
    }

    it("should report a test as ignored when Some(Ignore) is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.Ignore")), Set("org.scalatest.Ignore"))
      validateIgnoreBehavior(filter)
    }
    
    def validateIgnoreBehaviorDynamic(filter: Filter) {
      val filtered = filter(Set("myTestName"), Map.empty[String, Set[String]], suite)
      assert(filtered exists (tuple => tuple._1 == "myTestName"), "myTestName was in the tags map, but did not show up in the result of apply") 
      assert(filtered exists (tuple => tuple._1 == "myTestName" && tuple._2 == true), "myTestName was in the result of apply, but was not marked as ignored") 
    }
    
    it("should report a test DYNAMICALLY tagged as Ignored as ignored when None is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(None, Set("no ignore here"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore")))))
      validateIgnoreBehaviorDynamic(filter)
    }
    
    it("should report a test DYNAMICALLY tagged as Ignored as ignored when None is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(None, Set("org.scalatest.Ignore"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore")))))
      validateIgnoreBehaviorDynamic(filter)
    }

    it("should report a test DYNAMICALLY tagged as Ignored as ignored when Some(Ignore) is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.Ignore")), Set("no ignore here"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore")))))
      validateIgnoreBehavior(filter)
    }

    it("should report a test DYNAMICALLY tagged as Ignored as ignored when Some(Ignore) is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.Ignore")), Set("org.scalatest.Ignore"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore")))))
      validateIgnoreBehavior(filter)
    }

    def validateIgnoreOtherBehavior(filter: Filter) {
      val filtered = filter(Set("myTestName"), Map("myTestName" -> Set("org.scalatest.Ignore", "Other")), suite)
      assert(filtered exists (tuple => tuple._1 == "myTestName"), "myTestName was in the tags map, but did not show up in the result of apply") 
      assert(filtered exists (tuple => tuple._1 == "myTestName" && tuple._2 == true), "myTestName was in the result of apply, but was not marked as ignored") 
    }

    it("should report a test tagged as Other as ignored when Some(Other) is passed to filter" +
            "for tagsToInclude, and org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("Other")), Set("no ignore here"))
      validateIgnoreOtherBehavior(filter)
    }

    it("should report a test tagged as Other as ignored when Some(Other) is passed to filter" +
            "for tagsToInclude, and org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("Other")), Set("org.scalatest.Ignore"))
      validateIgnoreOtherBehavior(filter)
    }

    def validateIgnoreOtherBehaviorDynamic(filter: Filter) {
      val filtered = filter(Set("myTestName"), Map.empty[String, Set[String]], suite)
      assert(filtered exists (tuple => tuple._1 == "myTestName"), "myTestName was in the tags map, but did not show up in the result of apply") 
      assert(filtered exists (tuple => tuple._1 == "myTestName" && tuple._2 == true), "myTestName was in the result of apply, but was not marked as ignored") 
    }

    it("should report a test DYNAMICALLY tagged as Other as ignored when Some(Other) is passed to filter" +
            "for tagsToInclude, and org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("Other")), Set("no ignore here"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore", "Other")))))
      validateIgnoreOtherBehaviorDynamic(filter)
    }

    it("should report a test DYNAMICALLY tagged as Other as ignored when Some(Other) is passed to filter" +
            "for tagsToInclude, and org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("Other")), Set("org.scalatest.Ignore"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore", "Other")))))
      validateIgnoreOtherBehaviorDynamic(filter)
    }

    def validateNotReportingIgnoresBehavior(filter: Filter) {
      val filtered = filter(Set("myTestName"), Map("myTestName" -> Set("org.scalatest.Ignore")), suite)
      assert(!(filtered exists (tuple => tuple._1 == "myTestName")), "myTestName's Ignore tag was not in tagsToInclude, but showed up in the result of apply") 
    }

    it("should not report a test as ignored when Some(no ignore here) is passed to filter for" +
            "tagsToInclude, and org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("no ignore here")), Set("no ignore here"))
      validateNotReportingIgnoresBehavior(filter)
    }

    it("should not report a test as ignored when Some(no ignore here) is passed to filter for" +
            "tagsToInclude, and org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("no ignore here")), Set("org.scalatest.Ignore"))
      validateNotReportingIgnoresBehavior(filter)
    }
    
    def validateNotReportingIgnoresBehaviorDynamic(filter: Filter) {
      val filtered = filter(Set("myTestName"), Map.empty[String, Set[String]], suite)
      assert(!(filtered exists (tuple => tuple._1 == "myTestName")), "myTestName's Ignore tag was not in tagsToInclude, but showed up in the result of apply") 
    }
    
    it("should not report a test DYNAMICALLY tagged as Ignored as ignored when Some(no ignore here) is passed to filter for" +
            "tagsToInclude, and org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("no ignore here")), Set("no ignore here"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore")))))
      validateNotReportingIgnoresBehaviorDynamic(filter)
    }

    it("should not report a test DYNAMICALLY tagged as Ignored as ignored when Some(no ignore here) is passed to filter for" +
            "tagsToInclude, and org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("no ignore here")), Set("org.scalatest.Ignore"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore")))))
      validateNotReportingIgnoresBehaviorDynamic(filter)
    }

    it("should work properly when None is passed to filter for tagsToInclude") {
      
      // I want to pass None in for includes, pick a set of test names. From those test names, put some of them in the tags map, grabbing an arbitrary nonzero number of tags

      for (i <- 0 to 1000) {
        val testNames = potentialTestNames.drop(randomPositiveInt(potentialTestNames.length))
        val testNamesWithTags = testNames.drop(randomPositiveInt(testNames.length))
        //val tuples = for (testName <- testNamesWithTags) yield (testName, Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1)))
        val tuples =
          for (testName <- testNamesWithTags) yield
            (testName, Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1))) // subtract one, so never end up with an empty list
        val tags = Map() ++ tuples

        val tagsToExclude = Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length)) // Do want an empty set here occasionally
        val filter = new Filter(None, tagsToExclude)
        val filtered = filter(TreeSet[String]() ++ testNames, tags, suite)

        // Here I believe I was trying to check to make sure the test names come out in
        // the same order they went in, possibly with just some missing.
        assert(filtered.map(_._1).sortWith(_ < _) === filtered.map(_._1))

        for ((testName, ignore) <- filtered) {

          // testName should not be in the tagsToExclude map unless it is ignored
          if (tagsToExclude contains testName)
            assert(tags(testName) exists (_ == "org.scalatest.Ignore"), testName + " was in the filtered list and in the tags, but didn't have an Ignore tag")
        }

        // Check that every test name that is not at all in the tags map, should be in the filtered
        for (testName <- testNames) {
          if (!tags.contains(testName)) {
            assert(filtered exists (tuple => tuple._1 == testName), testName + " was not in the tags map, but did not show up in the result of apply") 
            assert(filtered exists (tuple => tuple._1 == testName && tuple._2 == false), testName + " was not in the tags map, and did show up in the result of apply, but was marked as ignored") 
          }
        }

        // Check that every test name that is in the tags as ignored, should be in the filtered as ignored,
        // unless it is also tagged with some other tag that is in tagsToExclude. In the latter case, the
        // other exclude tag should "overpower" the Ignore tag.
        for (testName <- testNames) {
          if (tags.contains(testName) && tags(testName).exists(_ == "org.scalatest.Ignore") &&
                  ((tags(testName) - "org.scalatest.Ignore") intersect tagsToExclude).isEmpty)
            assert(filtered exists (tuple => tuple._1 == testName && tuple._2 == true), testName + " was in the tags map as ignored, but did not show up in the result of apply marked as ignored") 
        }

        // Check that only the non-ignored tests are counted in the runnableTestsCount
        val runnableTests =
          for {
            (testName, ignore) <- filtered
            if !ignore
          } yield testName

        assert(filter.runnableTestCount(Set() ++ testNames, tags, suite) === runnableTests.size, "runnableTests = " + runnableTests + ", testNames = " + testNames + ", tags = " + tags + ", tagsToExclude = " + tagsToExclude)
      }
    }
    
    it("should work properly when None is passed to filter for tagsToInclude, when using dynamic tag") {
      // I want to pass None in for includes, pick a set of test names. From those test names, put some of them in the tags map, grabbing an arbitrary nonzero number of tags

      for (i <- 0 to 1000) {
        val testNames = potentialTestNames.drop(randomPositiveInt(potentialTestNames.length))
        val testNamesWithTags = testNames.drop(randomPositiveInt(testNames.length))
        //val tuples = for (testName <- testNamesWithTags) yield (testName, Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1)))
        val tuples =
          for (testName <- testNamesWithTags) yield
            (testName, Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1))) // subtract one, so never end up with an empty list
        val tags = Map() ++ tuples

        val tagsToExclude = Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length)) // Do want an empty set here occasionally
        val filter = new Filter(None, tagsToExclude, true, DynaTags(Map.empty, Map("testSuiteId" -> tags)))
        val filtered = filter(TreeSet[String]() ++ testNames, Map.empty[String, Set[String]], suite)

        // Here I believe I was trying to check to make sure the test names come out in
        // the same order they went in, possibly with just some missing.
        assert(filtered.map(_._1).sortWith(_ < _) === filtered.map(_._1))

        for ((testName, ignore) <- filtered) {

          // testName should not be in the tagsToExclude map unless it is ignored
          if (tagsToExclude contains testName)
            assert(tags(testName) exists (_ == "org.scalatest.Ignore"), testName + " was in the filtered list and in the tags, but didn't have an Ignore tag")
        }

        // Check that every test name that is not at all in the tags map, should be in the filtered
        for (testName <- testNames) {
          if (!tags.contains(testName)) {
            assert(filtered exists (tuple => tuple._1 == testName), testName + " was not in the tags map, but did not show up in the result of apply") 
            assert(filtered exists (tuple => tuple._1 == testName && tuple._2 == false), testName + " was not in the tags map, and did show up in the result of apply, but was marked as ignored") 
          }
        }

        // Check that every test name that is in the tags as ignored, should be in the filtered as ignored,
        // unless it is also tagged with some other tag that is in tagsToExclude. In the latter case, the
        // other exclude tag should "overpower" the Ignore tag.
        for (testName <- testNames) {
          if (tags.contains(testName) && tags(testName).exists(_ == "org.scalatest.Ignore") &&
                  ((tags(testName) - "org.scalatest.Ignore") intersect tagsToExclude).isEmpty)
            assert(filtered exists (tuple => tuple._1 == testName && tuple._2 == true), testName + " was in the tags map as ignored, but did not show up in the result of apply marked as ignored") 
        }

        // Check that only the non-ignored tests are counted in the runnableTestsCount
        val runnableTests =
          for {
            (testName, ignore) <- filtered
            if !ignore
          } yield testName

        assert(filter.runnableTestCount(Set() ++ testNames, Map.empty[String, Set[String]], suite) === runnableTests.size, "runnableTests = " + runnableTests + ", testNames = " + testNames + ", tags = " + tags + ", tagsToExclude = " + tagsToExclude)
      }
    }

    it("should not include an excluded tag even if it also appears as an included tag") {
      val filter = new Filter(Some(Set("Slow")), Set("Slow"))
      val filtered = filter(Set("myTestName"), Map("myTestName" -> Set("Slow")), suite)
      assert(filtered.size === 0) 
    }
    
    it("should not include an excluded tag even if it also appears as an included tag, when using dynamic tag") {
      val filter = new Filter(Some(Set("Slow")), Set("Slow"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("Slow")))))
      val filtered = filter(Set("myTestName"), Map.empty[String, Set[String]], suite)
      assert(filtered.size === 0) 
    }

    it("should include an included tag if there are no excluded tags") {
      val filter = new Filter(Some(Set("Slow")), Set())
      val filtered = filter(Set("myTestName"), Map("myTestName" -> Set("Slow")), suite)
      assert(filtered.size === 1) 
    }
    
    it("should include an included tag if there are no excluded tags, when using dynamic tag") {
      val filter = new Filter(Some(Set("Slow")), Set(), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("Slow")))))
      val filtered = filter(Set("myTestName"), Map.empty[String, Set[String]], suite)
      assert(filtered.size === 1) 
    }

    it("should work properly when Some is passed to filter for tagsToInclude") {
      
      // I want to pass None in for includes, pick a set of test names. From those test names, put some of them in the tags map, grabbing an arbitrary nonzero number of tags

      for (i <- 0 to 1000) {
        val testNames = potentialTestNames.drop(randomPositiveInt(potentialTestNames.length))
        val testNamesWithTags = testNames.drop(randomPositiveInt(testNames.length))
        //val tuples = for (testName <- testNamesWithTags) yield (testName, Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1)))
        val tuples =
          for (testName <- testNamesWithTags) yield
            (testName, Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1))) // subtract one, so never end up with an empty list
        val tags = Map() ++ tuples

        val tagsToExclude = Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length)) // Do want an empty set here occasionally
        val tagsToInclude = Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1)) // Again, subtracting one to avoid an empty set, which is an illegal argument. 

        val filter = new Filter(Some(tagsToInclude), tagsToExclude)
        val filtered = filter(TreeSet[String]() ++ testNames, tags, suite)

        // Here I believe I was trying to check to make sure the test names come out in
        // the same order they went in, possibly with just some missing.
        assert(filtered.map(_._1).sortWith(_ < _) === filtered.map(_._1))

        // Anything that's not in the include tags should not appear in the output
        // Look at everything in the output, and make sure it is in the include tags
        for ((testName, _) <- filtered) {
          assert(tags contains testName, "tags did not contain as a key the test name: " + testName)
          val tagsForTestName = tags(testName)
          val intersection = tagsToInclude intersect tagsForTestName
          assert(intersection.size != 0, "None of the tags for the test name showed up in the non-empty tags to include set")
        }
        for ((testName, ignore) <- filtered) {

          // testName should not be in the tagsToExclude map unless it is ignored
          if (tagsToExclude contains testName)
            assert(tags(testName) exists (_ == "org.scalatest.Ignore"), testName + " was in the filtered list and in the tags, but didn't have an Ignore tag")
        }

        // Check that every test name that is not at all in the tags map, should not be in the filtered, because it has to be tagged by one of the tags in tagsToInclude
        for (testName <- testNames) {
          if (!tags.contains(testName)) {
            assert(!filtered.exists(tuple => tuple._1 == testName), testName + " was not in the tags map, but showed up in the result of apply even though tagsToInclude was a Some") 
          }
        }

        // Check that every test name that is in the tags as ignored, which also shared a tag in common
        // with tagsToInclude, should be in the filtered as ignored, unless it is also tagged with some
        // other tag that is in tagsToExclude. In the latter case, the
        // other exclude tag should "overpower" the Ignore tag.
        for (testName <- testNames) {
          if (tags.contains(testName) && tags(testName).exists(_ == "org.scalatest.Ignore") &&
                  ((tags(testName) intersect tagsToInclude).size > 0) &&
                  ((tags(testName) - "org.scalatest.Ignore") intersect tagsToExclude).isEmpty)
            assert(filtered exists (tuple => tuple._1 == testName && tuple._2 == true), testName + " was in the tags map as ignored, but did not show up in the result of apply marked as ignored") 
        }
      }
    }
    
    it("should work properly when Some is passed to filter for tagsToInclude, when using dynamic tag") {
      
      // I want to pass None in for includes, pick a set of test names. From those test names, put some of them in the tags map, grabbing an arbitrary nonzero number of tags

      for (i <- 0 to 1000) {
        val testNames = potentialTestNames.drop(randomPositiveInt(potentialTestNames.length))
        val testNamesWithTags = testNames.drop(randomPositiveInt(testNames.length))
        //val tuples = for (testName <- testNamesWithTags) yield (testName, Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1)))
        val tuples =
          for (testName <- testNamesWithTags) yield
            (testName, Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1))) // subtract one, so never end up with an empty list
        val tags = Map() ++ tuples

        val tagsToExclude = Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length)) // Do want an empty set here occasionally
        val tagsToInclude = Set() ++ potentialTagNames.drop(randomPositiveInt(potentialTagNames.length - 1)) // Again, subtracting one to avoid an empty set, which is an illegal argument. 

        val filter = new Filter(Some(tagsToInclude), tagsToExclude, true, DynaTags(Map.empty, Map("testSuiteId" -> tags)))
        val filtered = filter(TreeSet[String]() ++ testNames, Map.empty[String, Set[String]], suite)

        // Here I believe I was trying to check to make sure the test names come out in
        // the same order they went in, possibly with just some missing.
        assert(filtered.map(_._1).sortWith(_ < _) === filtered.map(_._1))

        // Anything that's not in the include tags should not appear in the output
        // Look at everything in the output, and make sure it is in the include tags
        for ((testName, _) <- filtered) {
          assert(tags contains testName, "tags did not contain as a key the test name: " + testName)
          val tagsForTestName = tags(testName)
          val intersection = tagsToInclude intersect tagsForTestName
          assert(intersection.size != 0, "None of the tags for the test name showed up in the non-empty tags to include set")
        }
        for ((testName, ignore) <- filtered) {

          // testName should not be in the tagsToExclude map unless it is ignored
          if (tagsToExclude contains testName)
            assert(tags(testName) exists (_ == "org.scalatest.Ignore"), testName + " was in the filtered list and in the tags, but didn't have an Ignore tag")
        }

        // Check that every test name that is not at all in the tags map, should not be in the filtered, because it has to be tagged by one of the tags in tagsToInclude
        for (testName <- testNames) {
          if (!tags.contains(testName)) {
            assert(!filtered.exists(tuple => tuple._1 == testName), testName + " was not in the tags map, but showed up in the result of apply even though tagsToInclude was a Some") 
          }
        }

        // Check that every test name that is in the tags as ignored, which also shared a tag in common
        // with tagsToInclude, should be in the filtered as ignored, unless it is also tagged with some
        // other tag that is in tagsToExclude. In the latter case, the
        // other exclude tag should "overpower" the Ignore tag.
        for (testName <- testNames) {
          if (tags.contains(testName) && tags(testName).exists(_ == "org.scalatest.Ignore") &&
                  ((tags(testName) intersect tagsToInclude).size > 0) &&
                  ((tags(testName) - "org.scalatest.Ignore") intersect tagsToExclude).isEmpty)
            assert(filtered exists (tuple => tuple._1 == testName && tuple._2 == true), testName + " was in the tags map as ignored, but did not show up in the result of apply marked as ignored") 
        }
      }
    }

    describe("(when invoking the apply method that takes one test name)") {

      val emptyMap = Map[String, Set[String]]()

      it("should return (false, false) if tagsToInclude is None and tagsToExclude is empty" +
              "and the test has no tags") {
        val filter = new Filter(None, Set[String]())
        assert(filter("myTestName", emptyMap, suite) === (false, false))
      }
      
      it("should return (true, false) if tagsToInclude is None and tagsToExclude includes" +
              "SlowAsMolasses and the test is marked as SlowAsMolasses") {
        val filter = new Filter(None, Set("SlowAsMolasses"))
        assert(filter("myTestName", Map("myTestName" -> Set("SlowAsMolasses")), suite) === (true, false))
      }
      it("should return (true, false) if tagsToInclude is None and tagsToExclude includes" +
              "SlowAsMolasses and the test is DYNAMICALLY marked as SlowAsMolasses") {
        val filter = new Filter(None, Set("SlowAsMolasses"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("SlowAsMolasses")))))
        assert(filter("myTestName", Map.empty[String, Set[String]], suite) === (true, false))
      }
      
      it("should return (false, true) if tagsToInclude is None and tagsToExclude is empty" +
              "and the test is marked as ignored") {
        val filter = new Filter(None, Set[String]())
        assert(filter("myTestName", Map("myTestName" -> Set("org.scalatest.Ignore")), suite) === (false, true))
      }
      it("should return (false, true) if tagsToInclude is None and tagsToExclude is empty" +
              "and the test is DYNAMICALLY marked as ignored") {
        val filter = new Filter(None, Set[String](), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("org.scalatest.Ignore")))))
        assert(filter("myTestName", Map.empty[String, Set[String]], suite) === (false, true))
      }
      
      it("should return (true, false) if tagsToInclude is None and tagsToExclude includes" +
              "SlowAsMolasses and the test is marked as SlowAsMolasses and ignored") {
        val filter = new Filter(None, Set("SlowAsMolasses"))
        assert(filter("myTestName", Map("myTestName" -> Set("SlowAsMolasses", "org.scalatest.Ignore")), suite) === (true, false))
      }
      it("should return (true, false) if tagsToInclude is None and tagsToExclude includes" +
              "SlowAsMolasses and the test is DYNAMICALLY marked as SlowAsMolasses and ignored") {
        val filter = new Filter(None, Set("SlowAsMolasses"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("SlowAsMolasses", "org.scalatest.Ignore")))))
        assert(filter("myTestName", Map.empty[String, Set[String]], suite) === (true, false))
      }

      it("should return (false, false) if tagsToInclude includes a tag for the test name and tagsToExclude" +
              "is empty and the test is marked as SlowAsMolasses") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set[String]())
        assert(filter("myTestName", Map("myTestName" -> Set("SlowAsMolasses")), suite) === (false, false))
      }
      it("should return (false, false) if tagsToInclude includes a tag for the test name and tagsToExclude" +
              "is empty and the test is DYNAMICALLY marked as SlowAsMolasses") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set[String](), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("SlowAsMolasses")))))
        assert(filter("myTestName", Map.empty[String, Set[String]], suite) === (false, false))
      }
      
      it("should return (true, false) if tagsToInclude includes a tag for the test name and tagsToExclude" +
              "includes SlowAsMolasses and the test is marked as SlowAsMolasses") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set("SlowAsMolasses"))
        assert(filter("myTestName", Map("myTestName" -> Set("SlowAsMolasses")), suite) === (true, false))
      }
      it("should return (true, false) if tagsToInclude includes a tag for the test name and tagsToExclude" +
              "includes SlowAsMolasses and the test is DYNAMICALLY marked as SlowAsMolasses") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set("SlowAsMolasses"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("SlowAsMolasses")))))
        assert(filter("myTestName", Map.empty[String, Set[String]], suite) === (true, false))
      }
      
      it("should return (false, true) if tagsToInclude includes a tag for the test name and tagsToExclude" +
              "is empty and the test is marked as ignored") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set[String]())
        assert(filter("myTestName", Map("myTestName" -> Set("SlowAsMolasses", "org.scalatest.Ignore")), suite) === (false, true))
      }
      it("should return (false, true) if tagsToInclude includes a tag for the test name and tagsToExclude" +
              "is empty and the test is DYNAMICALLY marked as ignored") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set[String](), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("SlowAsMolasses", "org.scalatest.Ignore")))))
        assert(filter("myTestName", Map.empty[String, Set[String]], suite )=== (false, true))
      }
      
      it("should return (true, false) if tagsToInclude includes a tag for the test name and tagsToExclude" +
              "includes SlowAsMolasses and the test is marked as SlowAsMolasses and ignored") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set("SlowAsMolasses"))
        assert(filter("myTestName", Map("myTestName" -> Set("SlowAsMolasses", "org.scalatest.Ignore")), suite) === (true, false))
      }
      it("should return (true, false) if tagsToInclude includes a tag for the test name and tagsToExclude" +
              "includes SlowAsMolasses and the test is DYNAMICALLY marked as SlowAsMolasses and ignored") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set("SlowAsMolasses"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("SlowAsMolasses", "org.scalatest.Ignore")))))
        assert(filter("myTestName", Map.empty[String, Set[String]], suite) === (true, false))
      }

      it("should return (true, false) if tagsToInclude includes SlowAsMolasses but test is marked as FastAsLight") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set[String]())
        assert(filter("myTestName", Map("myTestName" -> Set("FastAsLight")), suite) === (true, false))
      }
      it("should return (true, false) if tagsToInclude includes SlowAsMolasses but test is DYNAMICALLY marked as FastAsLight") {
        val filter = new Filter(Some(Set("SlowAsMolasses")), Set[String](), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("FastAsLight")))))
        assert(filter("myTestName", Map.empty[String, Set[String]], suite) === (true, false))
      }
     }
  }
  
  describe("A Filter's includeNestedSuites field") {
    
    it("should a default value of true") {
      val filter = new Filter(Some(Set("SlowAsMolasses")), Set[String]())
      assert(filter.includeNestedSuites)
    }
    
  }
  
  describe("A Filter's dynaTags field") {
    
    it("should have default value of empty Set for suiteTags and empty Map for testTags") {
      val filter = new Filter(Some(Set("SlowAsMolasses")), Set[String]())
      assert(filter.dynaTags.suiteTags == Map.empty)
      assert(filter.dynaTags.testTags == Map.empty)
    }
    
    it("should merge in test dynamic tags in Filter.apply and Filter.runnableTestCount with suiteId") {
      val filter = new Filter(Some(Set("FastAsLight")), Set("org.scalatest.Ignore"), true, DynaTags(Map.empty, Map("testSuiteId" -> Map("myTestName" -> Set("FastAsLight")))))
      
      assert(filter("myTestName", Map[String, Set[String]](), suite) === (false, false))
      assert(filter("myTestName", Map("myTestName" -> Set("FastAsLight")), suite) === (false, false))
      assert(filter("myTestName", Map("myTestName" -> Set("FastAsLight", "org.scalatest.Ignore")), suite) === (false, true))
      
      assert(filter(Set("myTestName"), Map[String, Set[String]](), suite) === List(("myTestName", false)))
      assert(filter(Set("myTestName"), Map("myTestName" -> Set("FastAsLight")), suite) === List(("myTestName", false)))
      assert(filter(Set("myTestName"), Map("myTestName" -> Set("FastAsLight", "org.scalatest.Ignore")), suite) === List(("myTestName", true)))
      
      assert(filter.runnableTestCount(Set("myTestName"), Map[String, Set[String]](), suite) === 1)
      assert(filter.runnableTestCount(Set("myTestName"), Map("myTestName" -> Set("FastAsLight")), suite) === 1)
      assert(filter.runnableTestCount(Set("myTestName"), Map("myTestName" -> Set("FastAsLight", "org.scalatest.Ignore")), suite) === 0)
    }
  }
  
  describe("(when invoking the apply method that takes list of Suite)") {
    
    def validateIgnoreBehavior(filter: Filter, suite: Suite) {
      val filtered = filter(List(suite))
      assert(filtered exists (tuple => tuple._1 == suite), "suite was in the tags, but did not show up in the result of apply") 
      assert(filtered exists (tuple => tuple._1 == suite && tuple._2 == true), "suite was in the result of apply, but was not marked as ignored") 
    }
    
    @Ignore
    class IgnoreSuite extends Suite 
    
    it("should report a suite marked with org.scalatest.Ignore as ignored when None is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(None, Set("no ignore here"))
      validateIgnoreBehavior(filter, new IgnoreSuite())
    }

    it("should report a suite marked with org.scalatest.Ignore as ignored when None is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(None, Set("org.scalatest.Ignore"))
      validateIgnoreBehavior(filter, new IgnoreSuite())
    }

    it("should report a suite marked with org.scalatest.Ignore as ignored when Some(Ignore) is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.Ignore")), Set("no ignore here"))
      validateIgnoreBehavior(filter, new IgnoreSuite())
    }

    it("should report a suite marked with org.scalatest.Ignore as ignored when Some(Ignore) is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.Ignore")), Set("org.scalatest.Ignore"))
      validateIgnoreBehavior(filter, new IgnoreSuite())
    }
    
    it("should report a suite marked as org.scalatest.Ignore as ignored when Some(no ignore here) is passed to filter for" +
            "tagsToInclude, and org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("no ignore here")), Set("no ignore here"))
      validateIgnoreBehavior(filter, new IgnoreSuite())
    }
    
    it("should report a suite marked as org.scalatest.Ignore as ignored when Some(no ignore here) is passed to filter for" +
            "tagsToInclude, and org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("no ignore here")), Set("org.scalatest.Ignore"))
      validateIgnoreBehavior(filter, new IgnoreSuite())
    }
    
    class DynamicIgnoreSuite extends Suite 
    val dynaIgnoreSuite = new DynamicIgnoreSuite()
    
    it("should report a suite DYNAMICALLY marked with org.scalatest.Ignore as ignored when None is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(None, Set("no ignore here"), true, DynaTags(Map(dynaIgnoreSuite.suiteId -> Set("org.scalatest.Ignore")), Map.empty))
      validateIgnoreBehavior(filter, dynaIgnoreSuite)
    }
    
    it("should report a suite DYNAMICALLY marked with org.scalatest.Ignore as ignored when None is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(None, Set("org.scalatest.Ignore"), true, DynaTags(Map(dynaIgnoreSuite.suiteId -> Set("org.scalatest.Ignore")), Map.empty))
      validateIgnoreBehavior(filter, dynaIgnoreSuite)
    }

    it("should report a suite DYNAMICALLY marked with org.scalatest.Ignore as ignored when Some(Ignore) is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.Ignore")), Set("no ignore here"), true, DynaTags(Map(dynaIgnoreSuite.suiteId -> Set("org.scalatest.Ignore")), Map.empty))
      validateIgnoreBehavior(filter, dynaIgnoreSuite)
    }

    it("should report a suite DYNAMICALLY marked with org.scalatest.Ignore as ignored when Some(Ignore) is passed to filter for tagsToInclude, and" +
            "org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.Ignore")), Set("org.scalatest.Ignore"), true, DynaTags(Map(dynaIgnoreSuite.suiteId -> Set("org.scalatest.Ignore")), Map.empty))
      validateIgnoreBehavior(filter, dynaIgnoreSuite)
    }
    
    it("should not report a suite DYNAMICALLY marked as org.scalatest.Ignore as ignored when Some(no ignore here) is passed to filter for" +
            "tagsToInclude, and org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("no ignore here")), Set("no ignore here"), true, DynaTags(Map(dynaIgnoreSuite.suiteId -> Set("org.scalatest.Ignore")), Map.empty))
      validateIgnoreBehavior(filter, dynaIgnoreSuite)
    }

    it("should not report a suite DYNAMICALLY marked as org.scalatest.Ignore as ignored when Some(no ignore here) is passed to filter for" +
            "tagsToInclude, and org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("no ignore here")), Set("org.scalatest.Ignore"), true, DynaTags(Map(dynaIgnoreSuite.suiteId -> Set("org.scalatest.Ignore")), Map.empty))
      validateIgnoreBehavior(filter, dynaIgnoreSuite)
    }
    
    def validateIgnoreOtherBehavior(filter: Filter, suite: Suite) {
      val filtered = filter(List(suite))
      assert(filtered exists (tuple => tuple._1 == suite), "suite was in the tags, but did not show up in the result of apply") 
      assert(filtered exists (tuple => tuple._1 == suite && tuple._2 == true), "suite was in the result of apply, but was not marked as ignored") 
    }
    
    @Ignore
    @SlowAsMolasses
    class IgnoreSlowAsMolasses extends Suite 

    it("should report a suite tagged as SlowAsMolasses as ignored when Some(SlowAsMolasses) is passed to filter" +
            "for tagsToInclude, and org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("no ignore here"))
      validateIgnoreOtherBehavior(filter, new IgnoreSlowAsMolasses())
    }

    it("should report a suite tagged as SlowAsMolasses as ignored when Some(SlowAsMolasses) is passed to filter" +
            "for tagsToInclude, and org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore"))
      validateIgnoreOtherBehavior(filter, new IgnoreSlowAsMolasses())
    }
    
    class DynamicIgnoreSlowAsMolasses extends Suite 
    val dynaIgnoreSlowAsMolasses = new DynamicIgnoreSlowAsMolasses()
    
    it("should report a suite DYNAMICALLY tagged as SlowAsMolasses as ignored when Some(SlowAsMolasses) is passed to filter" +
            "for tagsToInclude, and org.scalatest.Ignore is not passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("no ignore here"), true , DynaTags(Map(dynaIgnoreSlowAsMolasses.suiteId -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses")), Map.empty))
      validateIgnoreOtherBehavior(filter, dynaIgnoreSlowAsMolasses)
    }

    it("should report a suite DYNAMICALLY tagged as SlowAsMolasses as ignored when Some(SlowAsMolasses) is passed to filter" +
            "for tagsToInclude, and org.scalatest.Ignore is passed in the tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.Ignore"), true, DynaTags(Map(dynaIgnoreSlowAsMolasses.suiteId -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses")), Map.empty))
      validateIgnoreOtherBehavior(filter, dynaIgnoreSlowAsMolasses)
    }
    
  }
  
  describe("(when invoking the apply method that takes one suite)") {
    
    class NoTagSuite extends Suite
    val noTagSuite = new NoTagSuite()

    it("should return (false, false) if tagsToInclude is None and tagsToExclude is empty" +
            "and the suite has no tags") {
      val filter = new Filter(None, Set[String]())
      assert(filter(noTagSuite) === (false, false))
    }
    
    @SlowAsMolasses
    class SlowAsMolassesSuite extends Suite
    val slowAsMolassesSuite = new SlowAsMolassesSuite
      
    it("should return (true, false) if tagsToInclude is None and tagsToExclude includes" +
            "SlowAsMolasses and the suite is marked as SlowAsMolasses") {
      val filter = new Filter(None, Set("org.scalatest.SlowAsMolasses"))
      assert(filter(slowAsMolassesSuite) === (true, false))
    }
    it("should return (true, false) if tagsToInclude is None and tagsToExclude includes" +
            "SlowAsMolasses and the suite is DYNAMICALLY marked as SlowAsMolasses") {
      val filter = new Filter(None, Set("org.scalatest.SlowAsMolasses"), true, DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.SlowAsMolasses")), Map.empty))
      assert(filter(noTagSuite) === (true, false))
    }
    
    @Ignore
    class IgnoreSuite extends Suite
    val ignoreSuite = new IgnoreSuite()
      
    it("should return (false, true) if tagsToInclude is None and tagsToExclude is empty" +
            "and the suite is marked as ignored") {
      val filter = new Filter(None, Set[String]())
      assert(filter(ignoreSuite) === (false, true))
    }
    it("should return (false, true) if tagsToInclude is None and tagsToExclude is empty" +
            "and the suite is DYNAMICALLY marked as ignored") {
      val filter = new Filter(None, Set[String](), true, DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.Ignore")), Map.empty))
      assert(filter(noTagSuite) === (false, true))
    }
    
    @SlowAsMolasses
    @Ignore
    class SlowAsMolassesIgnoreSuite extends Suite
    val slowAsMolassesIgnoreSuite = new SlowAsMolassesIgnoreSuite()
      
    it("should return (true, false) if tagsToInclude is None and tagsToExclude includes" +
            "SlowAsMolasses and the suite is marked as SlowAsMolasses and ignored") {
      val filter = new Filter(None, Set("org.scalatest.SlowAsMolasses"))
      assert(filter(slowAsMolassesIgnoreSuite) === (true, false))
    }
    it("should return (true, false) if tagsToInclude is None and tagsToExclude includes" +
            "SlowAsMolasses and the suite is DYNAMICALLY marked as SlowAsMolasses and ignored") {
      val filter = new Filter(None, Set("org.scalatest.SlowAsMolasses"), true, DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), Map.empty))
      assert(filter(noTagSuite) === (true, false))
    }

    it("should return (false, false) if tagsToInclude includes SlowAsMolasses and tagsToExclude" +
            "is empty and the suite is marked as SlowAsMolasses") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set[String]())
      assert(filter(slowAsMolassesSuite) === (false, false))
    }
    it("should return (false, false) if tagsToInclude includes SlowAsMolasses and tagsToExclude" +
            "is empty and the suite is DYNAMICALLY marked as SlowAsMolasses") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set[String](), true, DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.SlowAsMolasses")), Map.empty))
      assert(filter(noTagSuite) === (false, false))
    }
      
    it("should return (true, false) if tagsToInclude includes SlowAsMolasses and tagsToExclude" +
            "includes SlowAsMolasses and the suite is marked as SlowAsMolasses") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.SlowAsMolasses"))
      assert(filter(slowAsMolassesSuite) === (true, false))
    }
    it("should return (true, false) if tagsToInclude includes SlowAsMolasses and tagsToExclude" +
            "includes SlowAsMolasses and the suite is DYNAMICALLY marked as SlowAsMolasses") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.SlowAsMolasses"), true, DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.SlowAsMolasses")), Map.empty))
      assert(filter(noTagSuite) === (true, false))
    }
      
    it("should return (false, true) if tagsToInclude includes SlowAsMolasses and tagsToExclude" +
            "is empty and the suite is marked as ignored") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set[String]())
      assert(filter(slowAsMolassesIgnoreSuite) === (false, true))
    }
    it("should return (false, true) if tagsToInclude includes SlowAsMolasses and tagsToExclude" +
            "is empty and the suite is DYNAMICALLY marked as ignored") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set[String](), true, DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), Map.empty))
      assert(filter(noTagSuite) === (false, true))
    }
      
    it("should return (true, false) if tagsToInclude includes SlowAsMolasses and tagsToExclude" +
            "includes SlowAsMolasses and the test is marked as SlowAsMolasses and ignored") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set("org.scalatest.SlowAsMolasses"))
      assert(filter(slowAsMolassesIgnoreSuite) === (true, false))
    }
    it("should return (false, true) if tagsToInclude includes SlowAsMolasses and tagsToExclude" +
            "includes SlowAsMolasses and the test is DYNAMICALLY marked as SlowAsMolasses and ignored") {
      val filter = new Filter(Some(Set("SlowAsMolasses")), Set("SlowAsMolasses"), true, DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.Ignore")), Map.empty))
      assert(filter(noTagSuite) === (false, true))
    }
    
    @FastAsLight
    class FastAsLightSuite extends Suite
    val fastAsLightSuite = new FastAsLightSuite()

    it("should return (false, false) if tagsToInclude includes SlowAsMolasses but test is marked as FastAsLight") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set[String]())
      // Filter.apply(Suite) doesn't look at tagsToInclude
      assert(filter(fastAsLightSuite) === (false, false))
    }
    it("should return (true, false) if tagsToInclude includes SlowAsMolasses but test is DYNAMICALLY marked as FastAsLight") {
      val filter = new Filter(Some(Set("org.scalatest.SlowAsMolasses")), Set[String](), true, DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.FastAsLight")), Map.empty))
      // Filter.apply(Suite) doesn't look at tagsToInclude
      assert(filter(noTagSuite) === (false, false))
    }
  }
  
  describe("(when invoke apply on test names with suite annotation)") {
    
    @FastAsLight
    class TagSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      @SlowAsMolasses
      def testMethod3() {}
      @FastAsLight
      def testMethod4() {}
    }
    
    class NoTagSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      def testMethod3() {}
      def testMethod4() {}
    }
    
    class InsideTagSuite extends Suite {
      def testMethod1() {}
      def testMethod2() {}
      @SlowAsMolasses
      def testMethod3() {}
      @FastAsLight
      def testMethod4() {}
    }
    
    val expectedTestTags = Map("testMethod1" -> Set("org.scalatest.FastAsLight"), 
                               "testMethod2" -> Set("org.scalatest.FastAsLight"), 
                               "testMethod3" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.FastAsLight"), 
                               "testMethod4" -> Set("org.scalatest.FastAsLight"))
    
    def assertTestTags(testTags: Map[String, Set[String]]) {
      assert(testTags.size == expectedTestTags.size)
      expectedTestTags.foreach { entry => 
        val testName = entry._1
        val expectedTagSet = entry._2
        if (testTags.isDefinedAt(testName)) {
          val tagSet = testTags.get(testName).get
          assert(tagSet == expectedTagSet)
        }
        else
          fail("Entry with test name '" + testName + "' not found in testTags.")
      }
    }
    
    it("should 'just-in-time' tag test correctly when suite annotation is included in tagsToInclude") {
      val filter = new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty)
      val tagSuite = new TagSuite()
      val tagSuiteTestTags = filter.mergeTestDynamicTags(tagSuite.testTags, tagSuite)
      assertTestTags(tagSuiteTestTags)
      
      val noTagSuite = new NoTagSuite
      val dynaFilter1 = new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty, true, 
                                   DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.FastAsLight")), 
                                            Map(noTagSuite.suiteId -> Map("testMethod3" -> Set("org.scalatest.SlowAsMolasses"), 
                                                                          "testMethod4" -> Set("org.scalatest.FastAsLight")))))
      val noTagSuiteTestTags = dynaFilter1.mergeTestDynamicTags(noTagSuite.testTags, noTagSuite)
      assertTestTags(noTagSuiteTestTags)
      
      val insideTagSuite = new InsideTagSuite
      val dynaFilter2 = new Filter(Some(Set("org.scalatest.FastAsLight")), Set.empty, true, 
                                   DynaTags(Map(insideTagSuite.suiteId -> Set("org.scalatest.FastAsLight")), 
                                            Map.empty))
      val insideTagSuiteTestTags = dynaFilter2.mergeTestDynamicTags(insideTagSuite.testTags, insideTagSuite)
      assertTestTags(insideTagSuiteTestTags)
    }
    
    it("should 'just-in-time' tag test correctly when suite annotation is included in tagsToExclude") {
      val filter = new Filter(None, Set("org.scalatest.FastAsLight"))
      val tagSuite = new TagSuite()
      val tagSuiteTestTags = filter.mergeTestDynamicTags(tagSuite.testTags, tagSuite)
      assertTestTags(tagSuiteTestTags)
      
      val noTagSuite = new NoTagSuite
      val dynaFilter1 = new Filter(None, Set("org.scalatest.FastAsLight"), true, 
                                   DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.FastAsLight")), 
                                            Map(noTagSuite.suiteId -> Map("testMethod3" -> Set("org.scalatest.SlowAsMolasses"), 
                                                                          "testMethod4" -> Set("org.scalatest.FastAsLight")))))
      val noTagSuiteTestTags = dynaFilter1.mergeTestDynamicTags(noTagSuite.testTags, noTagSuite)
      assertTestTags(noTagSuiteTestTags)
      
      val insideTagSuite = new InsideTagSuite
      val dynaFilter2 = new Filter(None, Set("org.scalatest.FastAsLight"), true, 
                                   DynaTags(Map(insideTagSuite.suiteId -> Set("org.scalatest.FastAsLight")), 
                                            Map.empty))
      val insideTagSuiteTestTags = dynaFilter2.mergeTestDynamicTags(insideTagSuite.testTags, insideTagSuite)
      assertTestTags(insideTagSuiteTestTags)
    }
    
    it("should 'just-in-time' tag test correctly when suite annotation is included in both tagsToInclude and tagsToExclude") {
      val filter = new Filter(Some(Set("org.scalatest.FastAsLight")), Set("org.scalatest.FastAsLight"))
      val tagSuite = new TagSuite()
      val tagSuiteTestTags = filter.mergeTestDynamicTags(tagSuite.testTags, tagSuite)
      assertTestTags(tagSuiteTestTags)
      
      val noTagSuite = new NoTagSuite
      val dynaFilter1 = new Filter(Some(Set("org.scalatest.FastAsLight")), Set("org.scalatest.FastAsLight"), true, 
                                   DynaTags(Map(noTagSuite.suiteId -> Set("org.scalatest.FastAsLight")), 
                                            Map(noTagSuite.suiteId -> Map("testMethod3" -> Set("org.scalatest.SlowAsMolasses"), 
                                                                          "testMethod4" -> Set("org.scalatest.FastAsLight")))))
      val noTagSuiteTestTags = dynaFilter1.mergeTestDynamicTags(noTagSuite.testTags, noTagSuite)
      assertTestTags(noTagSuiteTestTags)
      
      val insideTagSuite = new InsideTagSuite
      val dynaFilter2 = new Filter(Some(Set("org.scalatest.FastAsLight")), Set("org.scalatest.FastAsLight"), true, 
                                   DynaTags(Map(insideTagSuite.suiteId -> Set("org.scalatest.FastAsLight")), 
                                            Map.empty))
      val insideTagSuiteTestTags = dynaFilter2.mergeTestDynamicTags(insideTagSuite.testTags, insideTagSuite)
      assertTestTags(insideTagSuiteTestTags)
    }
  }
}
