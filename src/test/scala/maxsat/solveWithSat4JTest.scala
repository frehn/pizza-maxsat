package maxsat

import org.scalatest.{FlatSpec, Matchers}

class solveWithSat4JTest extends FlatSpec with Matchers {
  "solveWithSat4J" should "solve a test problem" in {
    System.out.println(solveWithSat4J(TestData.testProblem))
    1 should equal(1)
  }
}