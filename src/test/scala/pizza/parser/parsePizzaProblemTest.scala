package pizza.parser

import org.scalatest.{FlatSpec, Matchers}
import pizza.TestData.problem

class parsePizzaProblemTest extends FlatSpec with Matchers {
  "parsePizzaProblem" should "parse the example from pizza.pdf" in {
    val inStr = scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("a_example.in")).mkString
    val problem = parsePizzaProblem(inStr)

    problem should equal(problem)
  }
}
