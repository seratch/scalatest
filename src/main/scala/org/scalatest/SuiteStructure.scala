package org.scalatest

object SuiteStructure {
  sealed abstract class Node(parent: Option[Node]) 
  
  abstract class Branch(parent: Option[Node]) extends Node(parent) {
    var subNodes: List[Node] = Nil
  }
  
  case class TestLeaf(
    parent: Option[Node],
    testName: String, // The full test name
    testText: String // The last portion of the test name that showed up on an inner most nested level
  ) extends Node(parent)
  
  case class InfoLeaf(parent: Option[Node], message: String) extends Node(parent)
  
  case class DescriptionBranch(
    parent: Option[Node],
    descriptionText: String,
    childPrefix: Option[String] // If defined, put it at the beginning of any child descriptionText or testText 
  ) extends Branch(parent)
  
  case class Root extends Branch(None)
}