package pizza

object getSolutionStatus {
  def apply(data: PizzaProblemData, solution: PizzaSolution): SolutionStatus = {
    takeFirstDefinedOrElse(Seq(findOverlappingSlices(solution),
      findSlicesWithTooLittleIngredients(solution)(data.problem),
      findSlicesTooLarge(data.problem, solution),
      findNonExistentSlices(data, solution)
    ), SolutionValid())
  }

  private def takeFirstDefinedOrElse[T](seq: Seq[Option[T]], orElse: T): T =
    seq.find(_.isDefined).getOrElse(Some(orElse)).get

  private def findSlicesWithTooLittleIngredients(solution: PizzaSolution)(implicit problem: PizzaProblem): Option[TooLittleIngredients] =
    solution.slices.filter(!hasEnoughIngredients(_)) match {
      case slices if slices.nonEmpty => Some(TooLittleIngredients(slices))
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
      case slices if slices.nonEmpty => Some(OverlappingSlices(slices))
      case _ => None
    }
  }

  private def findSlicesTooLarge(problem: PizzaProblem, solution: PizzaSolution): Option[SlicesTooLarge] = {
    solution.slices.filter(slice => slice.height * slice.length > problem.maxCells) match {
      case slices if slices.nonEmpty => Some(SlicesTooLarge(slices))
      case _ => None
    }
  }

  private def findNonExistentSlices(data: PizzaProblemData, solution: PizzaSolution): Option[NonExistentSlices] =
    solution.slices.filter(slice => !data.allSlices.contains(slice)) match {
      case slices if slices.nonEmpty => Some(NonExistentSlices(slices))
      case _ => None
    }
}

case class IngredientCount(tomatoes: Int, mushrooms: Int)

sealed trait SolutionStatus

case class SolutionValid() extends SolutionStatus

case class SlicesTooLarge(slice: Seq[Slice]) extends SolutionStatus

case class NonExistentSlices(slice: Seq[Slice]) extends SolutionStatus

case class OverlappingSlices(slices: Seq[(Slice, Slice)]) extends SolutionStatus

case class TooLittleIngredients(slices: Seq[Slice]) extends SolutionStatus