package org.scalatest.examples.featurespec.pending

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

    Scenario("User presses power button when TV is off") (pending)

    Scenario("User presses power button when TV is on") {
      val tv = new TVSet
      tv.pressPowerButton()
      assert(tv.isOn)
      tv.pressPowerButton()
      assert(!tv.isOn)
    }
  }
}
