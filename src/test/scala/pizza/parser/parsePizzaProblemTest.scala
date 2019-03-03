package pizza.parser

import org.scalatest.{FlatSpec, Matchers}

class parsePizzaProblemTest extends FlatSpec with Matchers {
  "parsePizzaProblem" should "parse the example from pizza.pdf" in {
    val problem = parsePizzaProblem(scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("a_example.in")).mkString)

    problem should equal(pizza.TestData.problem)
  }
}
