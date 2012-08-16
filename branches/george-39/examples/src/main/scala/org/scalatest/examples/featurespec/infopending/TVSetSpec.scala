package org.scalatest.examples.featurespec.infopending

import org.scalatest._

class TVSet {
  private var on: Boolean = false

  def isOn: Boolean = on

  def pressPowerButton() {
    on = !on
  }
}

class TVSetSpec extends FeatureSpec with GivenWhenThen {

  info("As a TV set owner")
  info("I want to be able to turn the TV on and off")
  info("So I can watch TV when I want")
  info("And save energy when I'm not watching TV")

  feature("TV power button") {
    scenario("User presses power button when TV is off") {
      given("a TV that is switched off")
      when("the power button is pressed")
      then("the TV should switch on")
      pending
    }

    scenario("User presses power button when TV is on") {
      given("a TV that is switched on")
      when("the power button is pressed")
      then("the TV should switch off")
      pending
    }
  }
}



