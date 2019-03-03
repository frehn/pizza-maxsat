package maxsat

import org.scalatest.{FlatSpec, Matchers}

class maxSatProblemToDimacsTest extends FlatSpec with Matchers {
  "getNumberOfVariables" should "compute the number of variables" in {
    maxSatProblemToDimacs.getVariables(TestData.problem) should have size 3
  }
}
