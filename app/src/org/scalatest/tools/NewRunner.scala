/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.tools

import scala.collection.mutable.ListBuffer
import java.lang.reflect.Constructor
import java.lang.reflect.Modifier
import java.net.URL
import java.net.MalformedURLException
import java.net.URLClassLoader
import java.io.File
import java.io.IOException
import javax.swing.SwingUtilities
import java.util.concurrent.ArrayBlockingQueue
import org.scalatest.testng.TestNGWrapperSuite
import org.scalatest.junit.JUnit3WrapperSuite
import org.scalatest.tools.ui._
import org.apache.tools.ant.BuildException

/**
 * <p>
 * Application that runs a suite of tests.
 * The application accepts command line arguments that specify optional <em>user-defined properties</em>, an optional 
 * <em>runpath</em>, zero to many <code>Reporter</code>s, optional lists of test groups to include and/or exclude, zero to many
 * <code>Suite</code> class names, zero to many "members-only" <code>Suite</code> paths, zero to many "wildcard" <code>Suite</code> paths,
 * and zero to many TestNG XML config file paths.
 * All of these arguments are described in more detail below. Here's a summary:
 * </p>
 *
 * <p>
 * <code>scala [-classpath scalatest-&lt;version&gt;.jar:...] org.scalatest.tools.Runner [-D&lt;key&gt;=&lt;value&gt; [...]] [-p &lt;runpath&gt;] [reporter [...]] [-n &lt;includes&gt;] [-x &lt;excludes&gt;] [-c] [-s &lt;suite class name&gt; [...]] [-m &lt;members-only suite path&gt; [...]] [-w &lt;wildcard suite path&gt; [...]] [-t &lt;TestNG config file path&gt; [...]]</code>
 * </p>
 *
 * <p>
 * The simplest way to start <code>Runner</code> is to specify the directory containing your compiled tests as the sole element of the runpath, for example:
 * </p>
 *
 * <p>
 * <code>scala -classpath scalatest-&lt;version&gt;.jar org.scalatest.tools.Runner -p compiled_tests</code>
 * </p>
 *
 * <p>
 * Given the previous command, <code>Runner</code> will discover and execute all <code>Suite</code>s in the <code>compiled_tests</code> directory and its subdirectories,
 * and show results in graphical user interface (GUI).
 * </p>
 *
 * <p>
 * <strong>Specifying user-defined properties</strong>
 * </p>
 *
 * <p>
 * A user-defined property consists of a key and a value. The key may not begin with
 * &quot;org.scalatest.&quot;. User-defined properties may be specified on the command line.
 * Each property is denoted with a "-D", followed immediately by the key string, an &quot;=&quot;, and the value string.
 * For example:
 * </p>
 *
 * <p>
 * <code>-Ddbname=testdb -Dserver=192.168.1.188</code>
 * </p>
 *
 * <p>
 * <strong>Specifying a runpath</strong>
 * </p>
 *
 * <p>
 * A runpath is the list of filenames, directory paths, and/or URLs that <code>Runner</code>
 * uses to load classes for the running test. If runpath is specified, <code>Runner</code> creates
 * a custom class loader to load classes available on the runpath.
 * The graphical user interface reloads the test classes anew for each run
 * by creating and using a new instance of the custom class loader for each run.
 * The classes that comprise the test may also be made available on
 * the classpath, in which case no runpath need be specified.
 * </p>
 *
 * <p>
 * The runpath is specified with the <b>-p</b> option. The <b>-p</b> must be followed by a space,
 * a double quote (<code>"</code>), a white-space-separated list of
 * paths and URLs, and a double quote. If specifying only one element in the runpath, you can leave off
 * the double quotes, which only serve to combine a white-space separated list of strings into one
 * command line argument. Here's an example:
 * </p>
 *
 * <p>
 * <code>-p "serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar"</code>
 * </p>
 *
 * <p>
 * <strong>Specifying reporters</strong>
 * </p>
 *
 * <p>
 * Reporters can be specified  on the command line in any of the following
 * ways:
 * </p>
 *
 * <ul>
 * <li> <code><b>-g[configs...]</b></code> - causes display of a graphical user interface that allows
 *    tests to be run and results to be investigated
 * <li> <code><b>-f[configs...] &lt;filename&gt;</b></code> - causes test results to be written to
 *     the named file
 * <li> <code><b>-o[configs...]</b></code> - causes test results to be written to
 *     the standard output
 * <li> <code><b>-e[configs...]</b></code> - causes test results to be written to
 *     the standard error
 * <li> <code><b>-r[configs...] &lt;reporterclass&gt;</b></code> - causes test results to be reported to
 *     an instance of the specified fully qualified <code>Reporter</code> class name
 * </ul>
 *
 * <p>
 * The <code><b>[configs...]</b></code> parameter, which is used to configure reporters, is described in the next section.
 * </p>
 *
 * <p>
 * The <code><b>-r</b></code> option causes the reporter specified in
 * <code><b>&lt;reporterclass&gt;</b></code> to be
 * instantiated.
 * Each reporter class specified with a <b>-r</b> option must be public, implement
 * <code>org.scalatest.Reporter</code>, and have a public no-arg constructor.
 * Reporter classes must be specified with fully qualified names. 
 * The specified reporter classes may be
 * deployed on the classpath. If a runpath is specified with the
 * <code>-p</code> option, the specified reporter classes may also be loaded from the runpath.
 * All specified reporter classes will be loaded and instantiated via their no-arg constructor.
 * </p>
 *
 * <p>
 * For example, to run a suite named <code>MySuite</code> from the <code>mydir</code> directory
 * using two reporters, the graphical reporter and a file reporter
 * writing to a file named <code>"test.out"</code>, you would type:
 * </p>
 *
 * <p>
 * <code>java -jar scalatest.jar -p mydir <b>-g -f test.out</b> -s MySuite</code>
 * </p>
 *
 * <p>
 * The <code><b>-g</b></code>, <code><b>-o</b></code>, or <code><b>-e</b></code> options can
 * appear at most once each in any single command line.
 * Multiple appearances of <code><b>-f</b></code> and <code><b>-r</b></code> result in multiple reporters
 * unless the specified <code><b>&lt;filename&gt;</b></code> or <code><b>&lt;reporterclass&gt;</b></code> is
 * repeated. If any of <code><b>-g</b></code>, <code><b>-o</b></code>, <code><b>-e</b></code>,
 * <code><b>&lt;filename&gt;</b></code> or <code><b>&lt;reporterclass&gt;</b></code> are repeated on
 * the command line, the <code>Runner</code> will print an error message and not run the tests.
 * </p>
 *
 * <p>
 * <code>Runner</code> adds the reporters specified on the command line to a <em>dispatch reporter</em>,
 * which will dispatch each method invocation to each contained reporter. <code>Runner</code> will pass
 * the dispatch reporter to executed suites. As a result, every
 * specified reporter will receive every report generated by the running suite of tests.
 * If no reporters are specified, a graphical
 * runner will be displayed that provides a graphical report of
 * executed suites.
 * </p>
 *
 * <p>
 * <strong>Configuring Reporters</strong>
 * </p>
 *
 * <p>
 * Each reporter specification on the command line can include configuration characters. Configuration
 * characters
 * are specified immediately following the <code><b>-g</b></code>, <code><b>-o</b></code>,
 * <code><b>-e</b></code>, <code><b>-f</b></code>, or <code><b>-r</b></code>. Valid configuration
 * characters are:
 * </p>
 *
 * <ul>
 * <li> <code><b>Y</b></code> - present <code>runStarting</code> method invocations
 * <li> <code><b>Z</b></code> - present <code>testStarting</code> method invocations
 * <li> <code><b>T</b></code> - present <code>testSucceeded</code> method invocations
 * <li> <code><b>F</b></code> - present <code>testFailed</code> method invocations
 * <li> <code><b>G</b></code> - present <code>testIgnored</code> method invocations
 * <li> <code><b>U</b></code> - present <code>suiteStarting</code> method invocations
 * <li> <code><b>P</b></code> - present <code>suiteCompleted</code> method invocations
 * <li> <code><b>B</b></code> - present <code>suiteAborted</code> method invocations
 * <li> <code><b>I</b></code> - present <code>infoProvided</code> method invocations
 * <li> <code><b>S</b></code> - present <code>runStopped</code> method invocations
 * <li> <code><b>A</b></code> - present <code>runAborted</code> method invocations
 * <li> <code><b>R</b></code> - present <code>runCompleted</code> method invocations
 * </ul>
 *
 * <p>
 * Each reporter class has a default configuration. If no configuration
 * is specified on the command line for a particular reporter, that
 * reporter uses its default configuration. If a configuration is specified, <code>Runner</code> will present
 * to the configured reporter only those report types mentioned in the configuration characters. If the command
 * line includes argument <code>-oFAB</code>, for example, only <code>testFailed</code>, 
 * <code>runAborted</code>, and <code>suiteAborted</code> events will be reported to the standard output reporter.
 * </p>
 *
 * <p>
 * For example, to run a suite using two reporters, the graphical reporter (using its default
 * configuration) and a standard error reporter configured to print only test failures, run aborts, and
 * suite aborts, you would type:
 * </p>
 *
 * <p>
 * <code>scala -classpath scalatest-&lt;version&gt;.jar -p mydir <strong>-g -eFAB</strong> -s MySuite</code>
 * </p>
 *
 * <p>
 * Note that no white space is allowed between the reporter option and the initial configuration
 * parameters. So <code>"-e FAB"</code> will not work,
 * <code>"-eFAB"</code> will work.
 * </p>
 *
 * <p>
 * <strong>Specifying includes and excludes</strong>
 * </p>
 *
 * <p>
 * You can specify named groups of tests to include or exclude from a run. To specify includes,
 * use <code>-n</code> followed by a white-space-separated list of group names to include, surrounded by
 * double quotes. (The double quotes are not needed if specifying just one group.)  Similarly, to specify excludes, use <code>-x</code> followed by a white-space-separated
 * list of group names to exclude, surrounded by double quotes. (As before, the double quotes are not needed if specifying just one group.) If includes is not specified, then all tests
 * except those mentioned in the excludes group (and in the <code>Ignore</code> group), will be executed.
 * (In other words, an empty includes list is like a wildcard, indicating all tests be included.)
 * If includes is specified, then only those tests in groups mentioned in the argument following <code>-n</code>
 * and not mentioned in the excludes group, will be executed. For more information on test groups, see
 * the <a href="Suite.html">documentation for <code>Suite</code></a>. Here are some examples:
 * </p>
 *
 * <p>
 * <ul>
 * <li><code>-n CheckinTests</code></li>
 * <li><code>-n FunctionalTests -x SlowTests</code></li>
 * <li><code>-n "CheckinTests FunctionalTests"-x "SlowTests NetworkTests"</code></li>
 * </ul>
 * </p>
 *
 * <p>
 * <strong>Executing <code>Suite</code>s concurrently</strong>
 * </p>
 *
 * <p>
 * With the proliferation of multi-core architectures, and the often parallelizable nature of tests, it is useful to be able to run
 * tests concurrently. If you include <code>-c</code> on the command line, <code>Runner</code> will pass a <code>Distributor</code> to 
 * the <code>Suite</code>s you specify with <code>-s</code>. <code>Runner</code> will set up a thread pool to execute any <code>Suite</code>s
 * passed to the <code>Distributor</code>'s <code>put</code> method concurrently. Trait <code>Suite</code>'s implementation of
 * <code>runNestedSuites</code> will place any nested <code>Suite</code>s into this <code>Distributor</code>. Thus, if you have a <code>Suite</code>
 * of tests that must be executed sequentially, you should override <code>runNestedSuites</code> as described in the <a href="Distributor.html">documentation for <code>Distributor</code></a>.
 * </p>
 *
 * <p>
 * <strong>Specifying <code>Suite</code>s</strong>
 * </p>
 *
 * <p>
 * Suites are specified on the command line with a <b>-s</b> followed by the fully qualified
 * name of a <code>Suite</code> subclass, as in:
 * </p>
 *
 * <p>
 * <code>-s com.artima.serviceuitest.ServiceUITestkit</code>
 * </p>
 *
 * <p>
 * Each specified suite class must be public, a subclass of
 * <code>org.scalatest.Suite</code>, and contain a public no-arg constructor.
 * <code>Suite</code> classes must be specified with fully qualified names. 
 * The specified <code>Suite</code> classes may be
 * loaded from the classpath. If a runpath is specified with the
 * <code>-p</code> option, specified <code>Suite</code> classes may also be loaded from the runpath.
 * All specified <code>Suite</code> classes will be loaded and instantiated via their no-arg constructor.
 * </p>
 *
 * <p>
 * The runner will invoke <code>execute</code> on each instantiated <code>org.scalatest.Suite</code>,
 * passing in the dispatch reporter to each <code>execute</code> method.
 * </p>
 *
 * <p>
 * <code>Runner</code> is intended to be used from the command line. It is included in <code>org.scalatest</code>
 * package as a convenience for the user. If this package is incorporated into tools, such as IDEs, which take
 * over the role of runner, object <code>org.scalatest.tools.Runner</code> may be excluded from that implementation of the package.
 * All other public types declared in package <code>org.scalatest.tools.Runner</code> should be included in any such usage, however,
 * so client software can count on them being available.
 * </p>
 *
 * <p>
 * <strong>Specifying "members-only" and "wildcard" <code>Suite</code> paths</strong>
 * </p>
 *
 * <p>
 * If you specify <code>Suite</code> path names with <code>-m</code> or <code>-w</code>, <code>Runner</code> will automatically
 * discover and execute accessible <code>Suite</code>s in the runpath that are either a member of (in the case of <code>-m</code>)
 * or enclosed by (in the case of <code>-w</code>) the specified path. As used in this context, a <em>path</em> is a portion of a fully qualified name.
 * For example, the fully qualifed name <code>com.example.webapp.MySuite</code> contains paths <code>com</code>, <code>com.example</code>, and <code>com.example.webapp</code>.
 * The fully qualifed name <code>com.example.webapp.MyObject.NestedSuite</code> contains paths <code>com</code>, <code>com.example</code>,
 * <code>com.example.webapp</code>, and <code>com.example.webapp.MyObject</code>.
 * An <em>accessible <code>Suite</code></em> is a public class that extends <code>org.scalatest.Suite</code>
 * and defines a public no-arg constructor. Note that <code>Suite</code>s defined inside classes and traits do not have no-arg constructors,
 * and therefore won't be discovered. <code>Suite</code>s defined inside singleton objects, however, do get a no-arg constructor by default, thus
 * they can be discovered.
 * </p>
 *
 * <p>
 * For example, if you specify <code>-m com.example.webapp</code>
 * on the command line, and you've placed <code>com.example.webapp.RedSuite</code> and <code>com.example.webapp.BlueSuite</code>
 * on the runpath, then <code>Runner</code> will instantiate and execute both of those <code>Suite</code>s. The difference
 * between <code>-m</code> and <code>-w</code> is that for <code>-m</code>, only <code>Suite</code>s that are direct members of the named path
 * will be discovered. For <code>-w</code>, any <code>Suite</code>s whose fully qualified
 * name begins with the specified path will be discovered. Thus, if <code>com.example.webapp.controllers.GreenSuite</code>
 * exists on the runpath, invoking <code>Runner</code> with <code>-w com.example.webapp</code> will cause <code>GreenSuite</code>
 * to be discovered, because its fully qualifed name begins with <code>"com.example.webapp"</code>. But if you invoke <code>Runner</code>
 * with <code>-m com.example.webapp</code>, <code>GreenSuite</code> will <em>not</em> be discovered because it is directly
 * a member of <code>com.example.webapp.controllers</code>, not <code>com.example.webapp</code>.
 * </p>
 *
 * <p>
 * If you specify no <code>-s</code>, <code>-m</code>, or <code>-w</code> arguments on the command line to <code>Runner</code>, it will discover and execute all accessible <code>Suite</code>s
 * in the runpath.
 * </p>
 *
 * <p>
 * <strong>Specifying TestNG XML config file paths</strong>
 * </p>
 *
 * <p>
 * If you specify one or more file paths with <code>-t</code>, <code>Runner</code> will create a <code>org.scalatest.testng.TestNGWrapperSuite</code>,
 * passing in a <code>List</code> of the specified paths. When executed, the <code>TestNGWrapperSuite</code> will create one <code>TestNG</code> instance
 * and pass each specified file path to it for running. If you include <code>-t</code> arguments, you must include TestNG's jar file on the class path or runpath.
 * The <code>-t</code> argument will enable you to run existing <code>TestNG</code> tests, including tests written in Java, as part of a ScalaTest run.
 * You need not use <code>-t</code> to run suites written in Scala that extend <code>TestNGSuite</code>. You can simply run such suites with 
 * <code>-s</code>, <code>-m</code>, or </code>-w</code> parameters.
 * </p>
 *
 * @author Bill Venners
 * @author Josh Cough
 */
object NewRunner {

  // TODO: I don't think I'm enforcing that properties can't start with "org.scalatest"
  // TODO: I don't think I'm handling rejecting multiple -f/-r with the same arg. -f fred.txt -f fred.txt should
  // fail, as should -r MyReporter -r MyReporter. I'm failing on -o -o, -g -g, and -e -e, but the error messages
  // could indeed be nicer.
  /**
   * Runs a suite of tests, with optional GUI. See the main documentation for this singleton object for the details.
   */
  def main(args: Array[String]) {
    val (
      runpathList, //
      reportersList,
      suitesList, //
      propertiesMap,
      includes, //
      excludes, //
      concurrent, //
      membersOnlyList, //
      wildcardList, //
      testNGList
    ) = RunnerArgsParser.parseArgs(args)

    val scalatest = new ScalaTest(runpathList)
	scalatest.setSuites(suitesList)
	scalatest.setIncludes(includes)
	scalatest.setExcludes(excludes)
	scalatest.setConcurrent(concurrent)
    scalatest.setWildcards(wildcardList)
    scalatest.setMembersOnly(membersOnlyList)

    
    val fullReporterSpecs: ReporterSpecs =
      if (reportersList.isEmpty)
        // If no reporters specified, just give them a graphic reporter
        new ReporterSpecs(Some(GraphicReporterSpec(ReporterOpts.Set32(0))), Nil, None, None, Nil)
      else
        RunnerArgsParser.parseReporterArgsIntoSpecs(reportersList)
    
 
    // Not yet supported
    val recipeName: Option[String] = None

    // If there's a graphic reporter, we need to leave it out of
    // reporterSpecs, because we want to pass all reporterSpecs except
    // the graphic reporter's to the RunnerJFrame (because RunnerJFrame *is*
    // the graphic reporter).
    val reporterSpecs: ReporterSpecs =
      fullReporterSpecs.graphicReporterSpec match {
        case None => fullReporterSpecs
        case Some(grs) => {
          new ReporterSpecs(
            None,
            fullReporterSpecs.fileReporterSpecList,
            fullReporterSpecs.standardOutReporterSpec,
            fullReporterSpecs.standardErrReporterSpec,
            fullReporterSpecs.customReporterSpecList
          )
        }
      }

    fullReporterSpecs.graphicReporterSpec match {
      case Some(GraphicReporterSpec(configSet)) => { // if we are using the GUI
        val graphicConfigSet = if (configSet.isEmpty) ReporterOpts.allOptions else configSet
        NewRunnerJFrame.run(scalatest) 
      }
      case None => { // Run the test without a GUI
      	scalatest.run	
      }
    }
  }
}
