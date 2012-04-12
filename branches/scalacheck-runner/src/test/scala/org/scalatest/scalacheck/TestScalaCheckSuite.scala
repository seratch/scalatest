package org.scalatest.scalacheck
import org.scalacheck.Properties
import org.scalacheck.Prop

class TestScalaCheckSuite extends Properties("String") with ScalaCheckSuite {

  property("startsWith") = Prop.forAll((a: String, b: String) => (a+b).startsWith(a))

  property("endsWith") = Prop.forAll((a: String, b: String) => (a+b).endsWith(b))
  
  property("concat") = Prop.forAll((a: String, b: String) => 
    (a+b).length >= a.length && (a+b).length >= b.length
  )

  property("substring") = Prop.forAll((a: String, b: String) => 
    (a+b).substring(a.length) == b
  )

  property("substring") = Prop.forAll((a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  )
  
}