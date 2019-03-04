package pizza

import maxsat.TestData.problem
import util.rectangle

object TestData {
  /**
    * The example problem from pizza.pdf.
    */
  val problem = {
    val R = 3;
    val C = 5;
    val L = 1;
    val H = 6;

    val ingredients = rectangle((0 until C), (0 until R)).map { case (x, y) => {
      (x, y) -> (if (y == 0 || y == 2)
        Tomato()
      else if (1 <= x && x <= 3)
        Mushroom()
      else
        Tomato())
    }
    }.toMap

    PizzaProblem(R, C, L, H, ingredients)
  }

  val solution = PizzaSolution(Seq(
    Slice(Cell(0, 0), 2, 3),
    Slice(Cell(2, 0), 1, 3),
    Slice(Cell(3, 0), 2, 3))
  )

  val noSolutionOverlap = PizzaSolution(Seq(
    Slice(Cell(0, 0), 2, 3),
    Slice(Cell(0, 0), 3, 1)
  ))

  val noSolutionNoMushrooms = PizzaSolution(Seq(
    Slice(Cell(0, 0), 5, 1)
  ))

  val noSolutionSliceTooLarge = PizzaSolution(Seq(
    Slice(Cell(0, 1), 5, 2)
  ))

  val noSolutionSliceDoesNotExist = PizzaSolution(Seq(
    Slice(Cell(3, 0), 1, 1)
  ))

  lazy val problemData = pizzaProblemToData(problem)
}