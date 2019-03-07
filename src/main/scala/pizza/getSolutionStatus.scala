package pizza

import util.rectangle

object getSolutionStatus {
  def apply(data: PizzaProblemData, solution: PizzaSolution): SolutionStatus = {
    takeFirstDefinedOrElse(Seq(findOverlappingSlices(solution),
      findSlicesWithTooLittleIngredients(data.problem, solution),
      findSlicesTooLarge(data.problem, solution),
      findNonExistentSlices(data, solution)
    ), SolutionValid())
  }

  private def takeFirstDefinedOrElse[T](seq: Seq[Option[T]], orElse: T): T =
    seq.find(_.isDefined).getOrElse(Some(orElse)).get

  private def findSlicesWithTooLittleIngredients(problem: PizzaProblem, solution: PizzaSolution): Option[TooLittleIngredients] = solution.slices.filter(slice => {
    val count = rectangle(0 until slice.length, 0 until slice.height).foldLeft(IngredientCount(0, 0)) { case (result, (i, j)) =>
      problem.ingredient.lift(slice.upperLeft.x + i, slice.upperLeft.y + j) match {
        case Some(Tomato()) => IngredientCount(result.tomatos + 1, result.mushrooms)
        case Some(Mushroom()) => IngredientCount(result.tomatos, result.mushrooms + 1)
        case None => IngredientCount(result.tomatos, result.mushrooms)
      }
    }
    count.tomatos < problem.minIngredients || count.mushrooms < problem.minIngredients
  }) match {
    case slices if slices.length > 0 => Some(TooLittleIngredients(slices))
    case _ => None
  }

  private def findOverlappingSlices(solution: PizzaSolution): Option[OverlappingSlices] = {
    solution.slices.zipWithIndex.flatMap { case (slice1, i) =>
      solution.slices.drop(i + 1).find(slice2 => {
        doSlicesOverlap(slice1, slice2)
      }) match {
        case Some(slice2) => Seq((slice1, slice2))
        case None => Seq()
      }
    } match {
      case slices if slices.length > 0 => Some(OverlappingSlices(slices))
      case _ => None
    }
  }

  private def findSlicesTooLarge(problem: PizzaProblem, solution: PizzaSolution): Option[SlicesTooLarge] = {
    solution.slices.filter(slice => slice.height * slice.length > problem.maxCells) match {
      case slices if slices.length > 0 => Some(SlicesTooLarge(slices))
      case _ => None
    }
  }

  private def findNonExistentSlices(data: PizzaProblemData, solution: PizzaSolution): Option[NonExistentSlices] =
    solution.slices.filter(slice => !data.allSlices.contains(slice)) match {
      case slices if slices.length > 0 => Some(NonExistentSlices(slices))
      case _ => None
    }
}

case class IngredientCount(tomatos: Int, mushrooms: Int)

sealed trait SolutionStatus

case class SolutionValid() extends SolutionStatus

case class SlicesTooLarge(slice: Seq[Slice]) extends SolutionStatus

case class NonExistentSlices(slice: Seq[Slice]) extends SolutionStatus

case class OverlappingSlices(slices: Seq[(Slice, Slice)]) extends SolutionStatus

case class TooLittleIngredients(slices: Seq[Slice]) extends SolutionStatus