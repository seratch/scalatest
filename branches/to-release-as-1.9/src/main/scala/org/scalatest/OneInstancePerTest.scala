/*
 * Copyright 2001-2009 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

import OneInstancePerTest.RunTheTestInThisInstance
/**
 * Trait that facilitates a style of testing in which each test is run in its own instance
 * of the suite class to isolate each test from the side effects of the other tests in the
 * suite.
 *
 * <p>
 * If you mix this trait into a <code>Suite</code>, you can initialize shared reassignable
 * fixture variables as well as shared mutable fixture objects in the constructor of the
 * class. Because each test will run in its own instance of the class, each test will
 * get a fresh copy of the instance variables. This is the approach to test isolation taken,
 * for example, by the JUnit framework.
 * </p>
 *
 * <p>
 * Here's an example of <code>OneInstancePerTest</code> being used in a <code>FunSuite</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.FunSuite
 * import org.scalatest.OneInstancePerTest
 * import collection.mutable.ListBuffer
 * 
 * class MySuite extends FunSuite with OneInstancePerTest {
 * 
 *   val builder = new StringBuilder("ScalaTest is ")
 *   val buffer = new ListBuffer[String]
 * 
 *   test("easy") {
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 * 
 *   test("fun") {
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *   }
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait OneInstancePerTest extends AbstractSuite {
  
  this: Suite =>

  protected abstract override def runTest(testName: String, args: RunArgs) {
    if (args.configMap.contains(RunTheTestInThisInstance)) {
      val oneInstance = newInstance
      oneInstance.run(Some(testName), args)
    }
    else
      super.runTest(testName, args)
  }
  
  protected abstract override def runTests(testName: Option[String], args: RunArgs) {
    if (args.configMap.contains(RunTheTestInThisInstance)) {
      val newConfigMap = args.configMap.filter(entry => entry._1 != RunTheTestInThisInstance)
      runTest(testName.get, args.copy(configMap = newConfigMap))
    }
    else {
      val newConfigMap = args.configMap + (RunTheTestInThisInstance -> true)
      super.runTests(testName, args.copy(configMap = newConfigMap))
    }
  }

  /**
   * Construct a new instance of this <code>Suite</code>.
   *
   * <p>
   * This trait's implementation of <code>runTests</code> invokes this method to create
   * a new instance of this <code>Suite</code> for each test. This trait's implementation
   * of this method uses reflection to call <code>this.getClass.newInstance</code>. This
   * approach will succeed only if this <code>Suite</code>'s class has a public, no-arg
   * constructor. In most cases this is likely to be true, because to be instantiated
   * by ScalaTest's <code>Runner</code> a <code>Suite</code> needs a public, no-arg
   * constructor. However, this will not be true of any <code>Suite</code> defined as
   * an inner class of another class or trait, because every constructor of an inner
   * class type takes a reference to the enclosing instance. In such cases, and in
   * cases where a <code>Suite</code> class is explicitly defined without a public,
   * no-arg constructor, you will need to override this method to construct a new
   * instance of the <code>Suite</code> in some other way.
   * </p>
   *
   * <p>
   * Here's an example of how you could override <code>newInstance</code> to construct
   * a new instance of an inner class:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.Suite
   *
   * class Outer {
   *   class InnerSuite extends Suite with OneInstancePerTest {
   *     def testOne() {}
   *     def testTwo() {}
   *     override def newInstance = new InnerSuite
   *   }
   * }
   * </pre>
   */
  def newInstance: Suite with OneInstancePerTest =
    this.getClass.newInstance.asInstanceOf[Suite with OneInstancePerTest]
}

private[scalatest] object OneInstancePerTest {
  val RunTheTestInThisInstance = "org.scalatest.RunTheTestInThisInstance" 
}
