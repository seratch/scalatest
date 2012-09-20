import collection.mutable.ListBuffer
import io.Source
import java.io.{File, FileWriter, BufferedWriter}

/*
* Copyright 2001-2011 Artima, Inc.
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

object GenMatchers {

  def translateShouldToMust(shouldLine: String): String = {
    val temp1 = shouldLine.replaceAll("<code>must</code>", "<code>I_WAS_must_ORIGINALLY</code>")
    val temp2 = temp1.replaceAll("<!-- PRESERVE -->should", " I_MUST_STAY_SHOULD")
    val temp3 = temp2.replaceAll(
      "<a href=\"MustMatchers.html\"><code>MustMatchers</code></a>",
      "<a href=\"I_WAS_Must_ORIGINALLYMatchers.html\"><code>I_WAS_Must_ORIGINALLYMatchers</code></a>"
    )
    val temp4 = temp3.replaceAll("should", "must")
    val temp5 = temp4.replaceAll("Should", "Must")
    val temp6 = temp5.replaceAll("I_WAS_must_ORIGINALLY", "should")
    val temp7 = temp6.replaceAll("I_MUST_STAY_SHOULD", "should")
    temp7.replaceAll("I_WAS_Must_ORIGINALLY", "Should")
  }
  
  def genMatchers(targetDir: File, targetFileName: String, templateFileName: String, scalaVersion: String): File = {
    val matchersFile = new File(targetDir, targetFileName)
    val matchersWriter = new BufferedWriter(new FileWriter(matchersFile))
    try {
      val lines = Source.fromFile(new File(templateFileName)).getLines.toList
      for (line <- lines) {
        if (!scalaVersion.startsWith("2.10") && line.toLowerCase.indexOf("parameterless") >= 0 && line.trim.startsWith("implicit def "))
          matchersWriter.write("//" + line)
        else
          matchersWriter.write(line)
        matchersWriter.newLine()
      }
    }
    finally {
      matchersWriter.flush()
      matchersWriter.close()
      println("Generated " + matchersFile.getAbsolutePath)
    }
    matchersFile
  }
  
  def genMustMatchers(targetDir: File, targetFileName: String, templateFileName: String, scalaVersion: String): File = {
    val mustMatchersFile = new File(targetDir, targetFileName)
    val mustMatchersWriter = new BufferedWriter(new FileWriter(mustMatchersFile))
    try {
      val lines = Source.fromFile(new File(templateFileName)).getLines.toList
      for (line <- lines) {
        val mustLine = translateShouldToMust(line)
        if (!scalaVersion.startsWith("2.10") && mustLine.toLowerCase.indexOf("parameterless") >= 0 && mustLine.trim.startsWith("implicit def "))
          mustMatchersWriter.write("//" + mustLine)
        else
          mustMatchersWriter.write(mustLine)
        mustMatchersWriter.newLine()
      }
    }
    finally {
      mustMatchersWriter.flush()
      mustMatchersWriter.close()
      println("Generated " + mustMatchersFile.getAbsolutePath)
    }
    mustMatchersFile
  }

  def genMain(targetDir: File, scalaVersion: String) {
    targetDir.mkdirs()
    val matchersDir = new File(targetDir, "matchers")
    matchersDir.mkdirs()
    val junitDir = new File(targetDir, "junit")
    junitDir.mkdirs()

    genMatchers(matchersDir, "Matchers.scala", "project/Matchers.template", scalaVersion)
    genMatchers(matchersDir, "ShouldMatchers.scala", "project/ShouldMatchers.template", scalaVersion)
    genMatchers(matchersDir, "LengthMatchers.scala", "project/LengthMatchers.template", scalaVersion)
    genMatchers(matchersDir, "LengthShouldMatchers.scala", "project/LengthShouldMatchers.template", scalaVersion)
    genMatchers(matchersDir, "SizeMatchers.scala", "project/SizeMatchers.template", scalaVersion)
    genMatchers(matchersDir, "SizeShouldMatchers.scala", "project/SizeShouldMatchers.template", scalaVersion)
    
    
    
    /*val matchersFile = new File(matchersDir, "Matchers.scala")
    val matchersWriter = new BufferedWriter(new FileWriter(matchersFile))
    try {
      val lines = Source.fromFile(new File("project/Matchers.template")).getLines.toList
      for (line <- lines) {
        if (!scalaVersion.startsWith("2.10") && line.toLowerCase.indexOf("parameterless") >= 0 && line.trim.startsWith("implicit def "))
          matchersWriter.write("//" + line)
        else
          matchersWriter.write(line)
        matchersWriter.newLine()
      }
    }
    finally {
      matchersWriter.flush()
      matchersWriter.close()
      println("Generated " + matchersFile.getAbsolutePath)
    }
    
    val lengthMatchersFile = new File(matchersDir, "LengthMatchers.scala")
    val lengthMatchersWriter = new BufferedWriter(new FileWriter(lengthMatchersFile))
    try {
      val lines = Source.fromFile(new File("project/LengthMatchers.template")).getLines.toList
      for (line <- lines) {
        if (!scalaVersion.startsWith("2.10") && line.toLowerCase.indexOf("parameterless") >= 0 && line.trim.startsWith("implicit def "))
          lengthMatchersWriter.write("//" + line)
        else
          lengthMatchersWriter.write(line)
        lengthMatchersWriter.newLine()
      }
    }
    finally {
      lengthMatchersWriter.flush()
      lengthMatchersWriter.close()
      println("Generated " + lengthMatchersFile.getAbsolutePath)
    }

    val shouldMatchersFile = new File(matchersDir, "ShouldMatchers.scala")
    val shouldMatchersWriter = new BufferedWriter(new FileWriter(shouldMatchersFile))
    try {
      val lines = Source.fromFile(new File("project/ShouldMatchers.template")).getLines.toList
      for (line <- lines) {
        if (!scalaVersion.startsWith("2.10") && line.toLowerCase.indexOf("parameterless") >= 0 && line.trim.startsWith("implicit def "))
          shouldMatchersWriter.write("//" + line)
        else
          shouldMatchersWriter.write(line)
        shouldMatchersWriter.newLine()
      }
    }
    finally {
      shouldMatchersWriter.flush()
      shouldMatchersWriter.close()
      println("Generated " + shouldMatchersFile.getAbsolutePath)
    }
    
    val lengthShouldMatchersFile = new File(matchersDir, "LengthShouldMatchers.scala")
    val lengthShouldMatchersWriter = new BufferedWriter(new FileWriter(lengthShouldMatchersFile))
    try {
      val lines = Source.fromFile(new File("project/LengthShouldMatchers.template")).getLines.toList
      for (line <- lines) {
        if (!scalaVersion.startsWith("2.10") && line.toLowerCase.indexOf("parameterless") >= 0 && line.trim.startsWith("implicit def "))
          lengthShouldMatchersWriter.write("//" + line)
        else
          lengthShouldMatchersWriter.write(line)
        lengthShouldMatchersWriter.newLine()
      }
    }
    finally {
      lengthShouldMatchersWriter.flush()
      lengthShouldMatchersWriter.close()
      println("Generated " + lengthShouldMatchersFile.getAbsolutePath)
    }*/

    genMustMatchers(matchersDir, "MustMatchers.scala", "project/ShouldMatchers.template", scalaVersion)
    genMustMatchers(matchersDir, "LengthMustMatchers.scala", "project/LengthShouldMatchers.template", scalaVersion)
    genMustMatchers(matchersDir, "SizeMustMatchers.scala", "project/SizeShouldMatchers.template", scalaVersion)
    
    /*val mustMatchersFile = new File(matchersDir, "MustMatchers.scala")
    val mustMatchersWriter = new BufferedWriter(new FileWriter(mustMatchersFile))
    try {
      val lines = Source.fromFile(new File("project/ShouldMatchers.template")).getLines.toList
      for (line <- lines) {
        val mustLine = translateShouldToMust(line)
        if (!scalaVersion.startsWith("2.10") && mustLine.toLowerCase.indexOf("parameterless") >= 0 && mustLine.trim.startsWith("implicit def "))
          mustMatchersWriter.write("//" + mustLine)
        else
          mustMatchersWriter.write(mustLine)
        mustMatchersWriter.newLine()
      }
    }
    finally {
      mustMatchersWriter.flush()
      mustMatchersWriter.close()
      println("Generated " + mustMatchersFile.getAbsolutePath)
    }
    
    val lengthMustMatchersFile = new File(matchersDir, "LengthMustMatchers.scala")
    val lengthMustMatchersWriter = new BufferedWriter(new FileWriter(lengthMustMatchersFile))
    try {
      val lines = Source.fromFile(new File("project/LengthShouldMatchers.template")).getLines.toList
      for (line <- lines) {
        val mustLine = translateShouldToMust(line)
        if (!scalaVersion.startsWith("2.10") && mustLine.toLowerCase.indexOf("parameterless") >= 0 && mustLine.trim.startsWith("implicit def "))
          lengthMustMatchersWriter.write("//" + mustLine)
        else
          lengthMustMatchersWriter.write(mustLine)
        lengthMustMatchersWriter.newLine()
      }
    }
    finally {
      lengthMustMatchersWriter.flush()
      lengthMustMatchersWriter.close()
      println("Generated " + lengthMustMatchersFile.getAbsolutePath)
    }*/

    val mustMatchersForJUnitFile = new File(junitDir, "MustMatchersForJUnit.scala")
    val mustMatchersForJUnitWriter = new BufferedWriter(new FileWriter(mustMatchersForJUnitFile))
    try {
      val lines = Source.fromFile(new File("src/main/scala/org/scalatest/junit/ShouldMatchersForJUnit.scala")).getLines.toList
      for (line <- lines) {
        val mustLine = translateShouldToMust(line)
        mustMatchersForJUnitWriter.write(mustLine)
        mustMatchersForJUnitWriter.newLine()
      }
    }
    finally {
      mustMatchersForJUnitWriter.flush()
      mustMatchersForJUnitWriter.close()
      println("Generated " + mustMatchersForJUnitFile.getAbsolutePath)
    }
  }

  def genTest(targetBaseDir: File, scalaVersion: String) {
    val sourceBaseDir = new File("src/test/scala/org/scalatest")
    val matchersDir = new File(targetBaseDir, "matchers")
    matchersDir.mkdirs()
    val shouldFileNames =
      List(
        "ShouldBehaveLikeSpec.scala",
        "ShouldContainElementSpec.scala",
        "ShouldContainKeySpec.scala",
        "ShouldContainValueSpec.scala",
        "ShouldEqualSpec.scala",
        "ShouldHavePropertiesSpec.scala",
        "ShouldLengthSpec.scala",
        "ShouldOrderedSpec.scala",
        "ShouldSizeSpec.scala",
        // "ShouldStackSpec.scala", now in examples
        // "ShouldStackFlatSpec.scala",
        "ShouldBeASymbolSpec.scala",
        "ShouldBeAnSymbolSpec.scala",
        "ShouldBeMatcherSpec.scala",
        "ShouldBePropertyMatcherSpec.scala",
        "ShouldBeSymbolSpec.scala",
        "ShouldEndWithRegexSpec.scala",
        "ShouldEndWithSubstringSpec.scala",
        "ShouldFullyMatchSpec.scala",
        "ShouldIncludeRegexSpec.scala",
        "ShouldIncludeSubstringSpec.scala",
        "ShouldLogicalMatcherExprSpec.scala",
        "ShouldMatcherSpec.scala",
        "ShouldPlusOrMinusSpec.scala",
        "ShouldSameInstanceAsSpec.scala",
        "ShouldStartWithRegexSpec.scala",
        "ShouldStartWithSubstringSpec.scala",
        "ShouldBeNullSpec.scala"
      )

    for (shouldFileName <- shouldFileNames) {

      val mustFileName = shouldFileName.replace("Should", "Must")
      val mustFile = new File(matchersDir, mustFileName)
      val writer = new BufferedWriter(new FileWriter(mustFile))
      try {
        val shouldLines = Source.fromFile(new File(sourceBaseDir, "matchers/" + shouldFileName)).getLines().toList // for 2.8
        for (shouldLine <- shouldLines) {
          val mustLine = translateShouldToMust(shouldLine)
          writer.write(mustLine.toString)
          writer.newLine() // add for 2.8
        }
      }
      finally {
        writer.close()
        println("Generated " + mustFile.getAbsolutePath)
      }
    }

    val junitDir = new File(targetBaseDir, "junit")
    junitDir.mkdirs()
    val mustMatchersForJUnitWordSpecFile = new File(junitDir, "MustMatchersForJUnitWordSpec.scala")
    val writer = new BufferedWriter(new FileWriter(mustMatchersForJUnitWordSpecFile))
    try {
      val shouldLines = Source.fromFile(new File(sourceBaseDir, "junit/" + "ShouldMatchersForJUnitWordSpec.scala")).getLines().toList // for 2.8
      for (shouldLine <- shouldLines) {
        val mustLine = translateShouldToMust(shouldLine)
        writer.write(mustLine.toString)
        writer.newLine() // add for 2.8
      }
    }
    finally {
      writer.close()
      println("Generated " + mustMatchersForJUnitWordSpecFile.getAbsolutePath)
    }
  }

  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genMain(new File(targetDir + "/main/scala/org/scalatest/"), scalaVersion)
    genTest(new File("gen/" + targetDir + "/test/scala/org/scalatest/"), scalaVersion)
  }
}
