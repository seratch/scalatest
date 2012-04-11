package org.scalatest.specs
import org.scalatest.FunSuite
import org.scalatest.finders.Selection
import org.scalatest.finders.Finder
import org.scalatest.finders.LocationUtils
import org.scalatest.WrapWith
import org.specs.Specification
import org.scalatest.finders.ConstructorBlock
import org.scalatest.finders.MethodInvocation
import org.scalatest.finders.ToStringTarget
import org.scalatest.finders.AstNode

class Spec1FinderSuite extends FunSuite {
  
  def expectSelection(selectionOpt: Option[Selection], expectedClassName: String, expectedDisplayName: String, expectedTestNames: Array[String]) {
    assert(selectionOpt.getClass == classOf[Some[_]], "Test is None, expected className=" + expectedClassName + ", displayName=" + expectedDisplayName + ", testNames=" + expectedTestNames.deepToString)
    val selection = selectionOpt.get
    expect(expectedClassName)(selection.className)
    expect(expectedDisplayName)(selection.displayName)
    expect(expectedTestNames.deepToString)(selection.testNames.deepToString)
  }
  
  test("Spec1Finder should find test name for tests written in specs1, using should, can and in") {
    
    @WrapWith(classOf[Spec1Runner])
    class TestSpec1Runner extends Specification {
      "My system " should {
        "provides basic feature 1" in {
          
        }
        "provides basic feature 2" in {
          
        }
      }
      "My system also" can {
        "provides advanced feature 1" in {
          
        }
        "provides advanced feature 2" in {
          
        }
      }
    }
    
    val suiteClass = classOf[Spec1Runner]
    val finderOpt: Option[Finder] = LocationUtils.getFinder(suiteClass)
    assert(finderOpt.isDefined, "Finder not found for suite that wrapped with Spec1Runner.")
    val finder = finderOpt.get
    assert(finder.getClass == classOf[Spec1Finder], "Suite that wrapped with org.scalatest.specs.Spec1Runner should use Spec1Finder.")
    
    val constructorBlock = ConstructorBlock(suiteClass.getName, Array.empty)
    val mySystem = MethodInvocation(suiteClass.getName, ToStringTarget(suiteClass.getName, null, Array.empty, "My system"), constructorBlock, Array.empty, "should", ToStringTarget(suiteClass.getName, null, Array.empty, "{}"))
    val providesBasicFeature1 = MethodInvocation(suiteClass.getName, ToStringTarget(suiteClass.getName, null, Array.empty, "provides basic feature 1"), mySystem, Array.empty, "in", ToStringTarget(suiteClass.getName, null, Array.empty, "{}"))
    val providesBasicFeature2 = MethodInvocation(suiteClass.getName, ToStringTarget(suiteClass.getName, null, Array.empty, "provides basic feature 2"), mySystem, Array.empty, "in", ToStringTarget(suiteClass.getName, null, Array.empty, "{}"))
    val mySystemAlso = MethodInvocation(suiteClass.getName, ToStringTarget(suiteClass.getName, null, Array.empty, "My system also"), constructorBlock, Array.empty, "can", ToStringTarget(suiteClass.getName, null, Array.empty, "{}"))
    val providesAdvancedFeature1 = MethodInvocation(suiteClass.getName, ToStringTarget(suiteClass.getName, null, Array.empty, "provides advanced feature 1"), mySystemAlso, Array.empty, "in", ToStringTarget(suiteClass.getName, null, Array.empty, "{}"))
    val providesAdvancedFeature2 = MethodInvocation(suiteClass.getName, ToStringTarget(suiteClass.getName, null, Array.empty, "provides advanced feature 2"), mySystemAlso, Array.empty, "in", ToStringTarget(suiteClass.getName, null, Array.empty, "{}"))
    List[AstNode](constructorBlock, mySystem, providesBasicFeature1, providesBasicFeature2, mySystemAlso, providesAdvancedFeature1, providesAdvancedFeature2).foreach(_.parent)
    
    val mySystemSelection = finder.find(mySystem)
    expectSelection(mySystemSelection, suiteClass.getName, "My system", Array("My system should provides basic feature 1", "My system should provides basic feature 2"))
    val providesBasicFeature1Selection = finder.find(providesBasicFeature1)
    expectSelection(providesBasicFeature1Selection, suiteClass.getName, "My system should provides basic feature 1", Array("My system should provides basic feature 1"))
    val providesBasicFeature2Selection = finder.find(providesBasicFeature2)
    expectSelection(providesBasicFeature2Selection, suiteClass.getName, "My system should provides basic feature 2", Array("My system should provides basic feature 2"))
    val mySystemAlsoSelection = finder.find(mySystemAlso)
    expectSelection(mySystemAlsoSelection, suiteClass.getName, "My system also", Array("My system also can provides advanced feature 1", "My system also can provides advanced feature 2"))
    val providesAdvancedFeature1Selection = finder.find(providesAdvancedFeature1)
    expectSelection(providesAdvancedFeature1Selection, suiteClass.getName, "My system also can provides advanced feature 1", Array("My system also can provides advanced feature 1"))
    val providesAdvancedFeature2Selection = finder.find(providesAdvancedFeature2)
    expectSelection(providesAdvancedFeature2Selection, suiteClass.getName, "My system also can provides advanced feature 2", Array("My system also can provides advanced feature 2"))
  }

}