package org.scalatest.specs

import org.scalatest.ServiceProvider
import org.scalatest.fingerprints.Fingerprint
import org.scalatest.JSuite
import org.specs.Specification
import org.scalatest.fingerprints.SubclassFingerprint

class ScalaTestServiceProvider extends ServiceProvider {

  def fingerprints: Array[Fingerprint] = 
    Array(new SubclassFingerprint { def isModule = false; def superclassName = "org.specs.Specification" } )
  
  def adapt(clazz: Class[_]): JSuite = clazz match { // This is ugly, any better way?
    case specClass: Class[Specification] => new Specs1Suite(specClass)
    case _ => null
  }
  
  def supported(clazz: Class[_]): Boolean = classOf[Specification].isAssignableFrom(clazz)
  
}