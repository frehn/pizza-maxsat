import java.io.FileInputStream

import maxsat.solveWithSat4J
import pizza._
import pizza.parser.parsePizzaProblem

object Main extends App {
  override def main(args: Array[String]): Unit = {
    // Example from pizza.pdf
    //val problem = parsePizzaProblem(scala.io.Source.fromInputStream(new FileInputStream("src/main/resources/a_example.in")).mkString.split("\n"))
    //val problem = parsePizzaProblem(scala.io.Source.fromInputStream(new FileInputStream("src/main/resources/b_small.in")).mkString.split("\n"))
    val problem = parsePizzaProblem(scala.io.Source.fromInputStream(new FileInputStream("src/main/resources/c_medium.in")).mkString.split("\n"))

    solveWithSat4J(pizzaProblemToMaxSatProblem(problem)) match {
      case Some(maxSatSolution) => {
        //System.out.println(maxSatSolution)
        val solution = maxSatSolutionToPizzaSolution(maxSatSolution)
        System.out.println("SOLUTION")
        val validStr = if (isValid(solution)) "YES" else "NO"
        System.out.println(s"Solution is valid: $validStr")
        System.out.println(s"Number of slices: ${solution.slices.size}")
        val score = scoreSolution(solution)
        System.out.println(s"Number of cells on slices: $score")
        System.out.println(s"Number of cells not on slices: ${problem.C*problem.R - score}")
        System.out.println("DETAILS")
        System.out.println(solution.slices.map(slice => slice.toString).mkString("\n"))

      }
      case None => System.out.println("No solution found")
    }
  }
}
