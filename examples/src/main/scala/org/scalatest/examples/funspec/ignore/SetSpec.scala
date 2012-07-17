package org.scalatest.examples.funspec.ignore

import org.scalatest.FunSpec

class SetSpec extends FunSpec {
  
  describe("An empty Set") {
    ignore("should have size 0") {
      assert(Set.empty.size === 0)
    }
    
    it("should produce NoSuchElementException when head is invoked") {
      intercept[NoSuchElementException] {
        Set.empty.head
      }
    }
  }
}