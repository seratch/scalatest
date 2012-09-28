package org.scalatest.tools

import org.scalatest.ServiceProvider
import java.util.Properties
import org.scalatest.fingerprints.Fingerprint
import org.scalatest.JSuite
import org.scalatest.fingerprints.SubclassFingerprint
import org.scalatest.fingerprints.AnnotatedFingerprint
import scala.collection.JavaConversions._ 
import org.scalatest.fingerprints.DoNotDiscoverFingerprint

private[scalatest] class ServiceProviderManager(loader: ClassLoader) {
  
  val providers = discoverServiceProviders
  val (supportedMap, doNotDiscoverMap) = {
    val (supportedList, doNotDiscoverList) =
      providers.flatMap(p => p.fingerprints.map(f => (f, p))).partition(!_._1.isInstanceOf[DoNotDiscoverFingerprint])
    (Map.empty[Fingerprint, ServiceProvider] ++ supportedList, 
     Map.empty[DoNotDiscoverFingerprint, ServiceProvider] ++ doNotDiscoverList.map(t => (t._1.asInstanceOf[DoNotDiscoverFingerprint], t._2))) // safe cast
  }

  def discoverServiceProviders: IndexedSeq[ServiceProvider] ={
    val urlList = loader.getResources("META-INF/scalatest.properties")
    urlList.map { url =>  
      val connection = url.openConnection
      val in = connection.getInputStream
      val prop = new Properties
      prop.load(in)
      val providerClassName = prop.get("framework").asInstanceOf[String]
      val providerClass = loader.loadClass(providerClassName)
      providerClass.newInstance.asInstanceOf[ServiceProvider]
    }.toIndexedSeq
  }
  
  def matchModule(isModule: Boolean, clazz: Class[_]) = if (isModule) clazz.getName.endsWith("$") else !clazz.getName.endsWith("$") 
  
  def supported(clazz: Class[_]): Boolean = 
    supportedMap.exists{ case (fingerprint, provider) =>
      fingerprint match {
        case subClassFingerprint: SubclassFingerprint => 
          val subClass = loader.loadClass(subClassFingerprint.superclassName)
          subClass.isAssignableFrom(clazz) && matchModule(subClassFingerprint.isModule, clazz)
        case annotatedFingerprint: AnnotatedFingerprint => 
          clazz.getAnnotations.exists(_.getClass.getName == annotatedFingerprint.annotationName) && matchModule(annotatedFingerprint.isModule, clazz)
        case _ => false
      }
    }
  
  def discoverable(clazz: Class[_]): Boolean = 
    !doNotDiscoverMap.exists { case (fingerprint, provider) => 
      clazz.getAnnotations.exists(_.getClass.getName == fingerprint.annotationName)
    }
  
  def adapt(clazz: Class[_]): Option[JSuite] = {
    providers.find(_.supported(clazz)) match {
      case Some(provider) => 
        val jSuite = provider.adapt(clazz)
        if (jSuite == null) None else Some(jSuite)
      case None => 
        None
    }
  }
}