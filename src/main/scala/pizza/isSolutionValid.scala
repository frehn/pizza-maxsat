package pizza

import util.rectangle

object isSolutionValid {
  def apply(data: PizzaProblemData, solution: PizzaSolution): Boolean = {
    slicesDoNotOverlap(solution) &&
      slicesHaveEnoughIngredients(data.problem, solution) &&
      slicesSmallEnough(data.problem, solution) &&
      slicesExist(data, solution)
  }

  private def slicesHaveEnoughIngredients(problem: PizzaProblem, solution: PizzaSolution): Boolean = solution.slices.exists(slice => {
    val count = rectangle(0 until slice.length, 0 until slice.width).foldLeft(IngredientCount(0, 0)) { case (result, (i, j)) =>
      problem.ingredient(slice.upperLeft.x + i, slice.upperLeft.y + j) match {
        case Tomato() => IngredientCount(result.tomatos + 1, result.mushrooms)
        case Mushroom() => IngredientCount(result.tomatos, result.mushrooms + 1)
      }
    }
    count.tomatos >= problem.L && count.mushrooms >= problem.L
  })

  private def slicesDoNotOverlap(solution: PizzaSolution) = {
    !solution.slices.zipWithIndex.exists { case (slice1, i) =>
      solution.slices.drop(i + 1).exists(slice2 => {
        doSlicesOverlap(slice1, slice2)
      })
    }
  }

  private def slicesSmallEnough(problem: PizzaProblem, solution: PizzaSolution): Boolean = {
    solution.slices.forall(slice => slice.width * slice.length <= problem.H)
  }

  private def slicesExist(data: PizzaProblemData, solution: PizzaSolution): Boolean =
    (solution.slices.toSet -- data.allSlices).isEmpty
}

case class IngredientCount(tomatos: Int, mushrooms: Int)