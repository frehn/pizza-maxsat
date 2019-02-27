import maxsat.solveWithSat4J
import pizza._

object Main extends App {
  override def main(args: Array[String]): Unit = {
    // Example from pizza.pdf
    val testProblem = PizzaProblem(R = 3,
      C = 5,
      L = 1,
      H = 6,
      (x, y) => {
        if (x == 0 || x == 2)
          Tomato()
        else if (1 <= y && y <= 3)
          Mushroom()
        else
          Tomato()
      })

    solveWithSat4J(pizzaProblemToMaxSatProblem(testProblem)) match {
      case Some(maxSatSolution) => {
        //System.out.println(maxSatSolution)
        val solution = maxSatSolutionToPizzaSolution(maxSatSolution)
        System.out.println("SOLUTION")
        val validStr = if (isValid(solution)) "YES" else "NO"
        System.out.println(s"Solution is valid: $validStr")
        System.out.println(s"Number of slices: ${solution.slices.size}")
        val score = scoreSolution(solution)
        System.out.println(s"Number of cells on slices: $score")
        System.out.println(s"Number of cells not on slices: ${testProblem.C*testProblem.R - score}")
        System.out.println("DETAILS")
        System.out.println(solution.slices.map(slice => slice.toString).mkString("\n"))

      }
      case None => System.out.println("No solution found")
    }
  }
}
