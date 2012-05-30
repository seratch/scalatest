package org.scalatest.tools

import javassist.ClassPool
import java.net.URLClassLoader
import java.net.URL
import javassist.NotFoundException
import javassist.LoaderClassPath
import javassist.CtClass
import java.io.BufferedInputStream
import java.io.File

private[tools] class ScalaTestLoader(runPath: List[String], urlList: List[URL], parent: ClassLoader) extends ClassLoader {

  private val urlLoader = new URLClassLoader(urlList.toArray, parent)
  
  private val pool = new ClassPool()
  pool.appendSystemPath()
  runPath.foreach(pool.appendClassPath(_))
  
  override protected def findClass(name: String): Class[_] = {
    try {
      val cc = pool.get(name)
      val b = cc.toBytecode()
      defineClass(name, b, 0, b.length)
    }
    catch {
      case e: NotFoundException => 
        val path = name.replace('.', '/').concat(".class")
        val cc = pool.makeClass(urlLoader.getResourceAsStream(path))
        val b = cc.toBytecode
        defineClass(name, b, 0, b.length)
    }
  }
  
  def getCtClass(name: String): CtClass = { 
    try {
      pool.get(name)
    }
    catch {
      case e: NotFoundException => 
        val path = name.replace('.', '/').concat(".class")
        val resourceStream = urlLoader.getResourceAsStream(path)
        if (resourceStream != null)
          pool.makeClass(resourceStream)
        else
          throw e
    }
  }
}