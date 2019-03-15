package maxsat

import java.io.{ByteArrayOutputStream, FileInputStream}

import org.scalatest.{FlatSpec, Matchers}
import pizza.parser.parsePizzaProblem
import pizza.{pizzaProblemToData, pizzaProblemToMaxSatProblem}

class maxSatProblemToDimacsTest extends FlatSpec with Matchers {
  "getNumberOfVariables" should "compute the number of variables" in {
    maxSatProblemToDimacs.getVariables(TestData.problem) should have size 3
  }

  "maxSatProblemToDimacs" should "produce a correct Dimacs string for b_small.in" in {
    val problem = parsePizzaProblem(scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("b_small.in")).mkString)
    val data = pizzaProblemToData(problem)
    val maxSatProblem = pizzaProblemToMaxSatProblem(data)

    val expected = scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("b_small.dimacs")).mkString

    val out = new ByteArrayOutputStream()
    maxSatProblemToDimacs(maxSatProblem, out)

    out.toString("UTF-8") should equal (expected)
  }
}
