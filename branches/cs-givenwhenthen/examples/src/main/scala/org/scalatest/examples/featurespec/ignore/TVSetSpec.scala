package org.scalatest.examples.featurespec.ignore

import org.scalatest.FeatureSpec

class TVSet {
  private var on: Boolean = false
  def isOn: Boolean = on
  def pressPowerButton() {
    on = !on
  }
}

class TVSetSpec extends FeatureSpec {

  Feature("TV power button") {
    ignore("User presses power button when TV is off") {
      val tv = new TVSet
      assert(!tv.isOn)
      tv.pressPowerButton()
      assert(tv.isOn)
    }

    Scenario("User presses power button when TV is on") {
      val tv = new TVSet
      tv.pressPowerButton()
      assert(tv.isOn)
      tv.pressPowerButton()
      assert(!tv.isOn)
    }
  }
}
