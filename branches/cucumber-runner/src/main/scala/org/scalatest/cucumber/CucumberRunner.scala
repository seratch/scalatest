package org.scalatest.cucumber

import org.scalatest.Suite
import org.scalatest.Reporter
import org.scalatest.Stopper
import org.scalatest.Filter
import org.scalatest.Tracker
import org.scalatest.Distributor
import cucumber.io.ClasspathResourceLoader
import cucumber.runtime.Runtime
import cucumber.runtime.model.CucumberFeature
import cucumber.runtime.model.CucumberScenario
import cucumber.runtime.model.CucumberScenarioOutline
import cucumber.runtime.model.CucumberTagStatement

class CucumberRunner(cucumberClass: Class[_]) extends Suite {
  
  

  override def run(testName: Option[String], reporter: Reporter, stopper: Stopper, filter: Filter,
              configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker) {
    if (testName == null)
      throw new NullPointerException("testName was null")
    if (reporter == null)
      throw new NullPointerException("reporter was null")
    if (stopper == null)
      throw new NullPointerException("stopper was null")
    if (filter == null)
      throw new NullPointerException("filter was null")
    if (configMap == null)
      throw new NullPointerException("configMap was null")
    if (distributor == null)
      throw new NullPointerException("distributor was null")
    if (tracker == null)
      throw new NullPointerException("tracker was null")

    val stopRequested = stopper
    val report = wrapReporterIfNecessary(reporter)
    
    val classLoader = cucumberClass.getClassLoader
    val resourceLoader = new ClasspathResourceLoader(classLoader)
    // TODO: Implement this
    //assertNoCucumberAnnotatedMethods(clazz)
    
    val runtimeOptionsFactory = new RuntimeOptionsFactory(cucumberClass)
    val runtimeOptions = runtimeOptionsFactory.create
    val runtime = new Runtime(resourceLoader, classLoader, runtimeOptions)
    
    // TODO: Create formatter(s) based on Annotations. Use same technique as in cli.Main for MultiFormatter
    val scalatestReporter = new ScalaTestReporter(runtimeOptions.reporter(classLoader), runtimeOptions.formatter(classLoader), reporter)
    
    val features: Array[CucumberFeature] = scala.collection.JavaConversions.asBuffer(runtimeOptions.cucumberFeatures(resourceLoader)).toArray
    features.foreach { feature => 
      val cucumberTagStatements = scala.collection.JavaConversions.asBuffer(feature.getFeatureElements).toArray
      cucumberTagStatements.foreach { cucumberTagStatement =>
        cucumberTagStatement match {
          case scenario: CucumberScenario => 
            scenario.run(scalatestReporter, scalatestReporter, runtime)
          case outline: CucumberScenarioOutline => 
            val examples = scala.collection.JavaConversions.asBuffer(outline.getCucumberExamplesList).toArray
            examples.foreach { example => 
              val scenarios = scala.collection.JavaConversions.asBuffer(example.createExampleScenarios).toArray
              scenarios.foreach { scenario => 
                scenario.run(scalatestReporter, scalatestReporter, runtime)
              }
            }
          case _ => 
            println("Unxpected cucumberTagStatement: " + cucumberTagStatement.getClass.getName)
        }
      }
    }
  }  
}