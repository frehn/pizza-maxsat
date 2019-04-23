package maxsat

import maxsat.solver.solveWithSat4J
import org.scalatest.{FlatSpec, Matchers}

class solveWithSat4JTest extends FlatSpec with Matchers {
  "solveWithSat4J" should "solve a test problem" in {
    solveWithSat4J(TestData.problem) should be ('defined)
  }
}