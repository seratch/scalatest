package org.scalatest.exceptions

import org.scalatest.Resources

class DuplicateSuiteIdException(suiteId: String, suiteClassName1: String, suiteClassName2: String) 
  extends RuntimeException(Resources("duplicateSuiteId", suiteId, suiteClassName1, suiteClassName2)) 