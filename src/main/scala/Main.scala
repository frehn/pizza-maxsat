import java.io.FileInputStream

import maxsat.solveWithSat4J
import pizza._
import pizza.parser.parsePizzaProblem

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val problem = parsePizzaProblem(scala.io.Source.fromInputStream(new FileInputStream(args(0))).mkString)
    val data = pizzaProblemToData(problem)

    solveWithSat4J(pizzaProblemToMaxSatProblem(data)) match {
      case Some(maxSatSolution) =>
        //System.out.println(maxSatSolution)
        val solution = maxSatSolutionToPizzaSolution(maxSatSolution)
        System.out.println("Solution")
        System.out.println("--------")
        val validStr = if (isSolutionValid(data, solution)) "YES" else "NO"
        System.out.println(s"Solution is valid: $validStr")
        System.out.println()
        System.out.println(s"Number of slices: ${solution.slices.size}")
        val score = scoreSolution(solution)
        System.out.println(s"Number of cells on slices: $score")
        System.out.println(s"Number of cells not on slices: ${problem.C*problem.R - score}")
        System.out.println()
        System.out.println("Details")
        System.out.println("-------")
        System.out.println(solution.slices.map(slice => slice.toString).mkString("\n"))
      case None => System.out.println("No solution found")
    }
  }
}
