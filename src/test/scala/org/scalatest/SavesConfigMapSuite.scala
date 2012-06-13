package org.scalatest

import SavesConfigMapSuite.theConfigMap

@WrapWith(classOf[ConfigMapWrapperSuite])
class SavesConfigMapSuite(configMap: Map[String, Any]) extends FunSuite {
  theConfigMap = Some(configMap)
  test("one test") {}
  test("two test") {}
  test("red test") {}
  test("blue test", org.scalatest.mytags.FastAsLight) {}
  ignore("ignore me") {}
  class NSuite(n: Int) extends Suite {
    override def suiteId = getClass.getName + n
  }
  override def nestedSuites: Vector[Suite] = Vector(new NSuite(1), new NSuite(2), new NSuite(3))
}

object SavesConfigMapSuite {
  private var theConfigMap: Option[Map[String, Any]] = None
  def savedConfigMap = theConfigMap
  def resetConfigMap() { theConfigMap = None }
}
