import maxsat.solveWithSat4J
import pizza._

object Main extends App {
  override def main(args: Array[String]): Unit = {
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
        System.out.println(maxSatSolution)
        System.out.println(maxSatSolutionToPizzaSolution(maxSatSolution))
      }
      case None => System.out.println("No solution found")
    }
  }
}
