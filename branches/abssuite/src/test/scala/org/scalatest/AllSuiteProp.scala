package org.scalatest

trait AllSuiteProp extends MethodSuiteProp with FunctionSuiteProp {

  override def examples =
    Table[AbstractSuite with FixtureServices](
      "suite",
      suite,
      fixtureSuite,
      junit3Suite, 
      junitSuite,
      testngSuite, 
      funSuite,
      fixtureFunSuite,
      funSpec,
      fixtureFunSpec,
      featureSpec,
      fixtureFeatureSpec,
      flatSpec,
      fixtureFlatSpec,
      freeSpec,
      fixtureFreeSpec,
      propSpec,
      fixturePropSpec,
      wordSpec,
      fixtureWordSpec, 
      pathFreeSpec, 
      pathFunSpec
    )
  
}