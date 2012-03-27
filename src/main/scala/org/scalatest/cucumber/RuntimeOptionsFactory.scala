package org.scalatest.cucumber

import cucumber.runtime.Utils.packagePath
import cucumber.runtime.RuntimeOptions

class RuntimeOptionsFactory(clazz: Class[_]) {
  
  def create = {
    val options = clazz.getAnnotation(classOf[CucumberOptions])
    val dryRun = 
      if (options != null && options.dryRun) 
        List("--dry-run") 
      else 
        List.empty[String]
        
      val glue = 
        if (options != null && options.glue.length > 0)
          options.glue.map(glue => List("--glue", glue)).flatten.toList
        else 
          List("--glue", packagePath(clazz))
            
      val tags = 
        if (options != null)
          options.tags.map(tags => List("--tags", tags)).flatten.toList
        else
          List.empty[String]
        
      val formats = 
        if (options != null && options.format.length != 0)
          options.format.map(format => List("--format", format)).flatten.toList
        else
          List("--format", "null")
            
      val features = 
        if (options != null && options.features.length != 0)
          options.features.toList
        else
          List(packagePath(clazz))
            
      val args = dryRun ++ glue ++ tags ++ formats ++ features
      
      new RuntimeOptions(args: _*)
  }
}