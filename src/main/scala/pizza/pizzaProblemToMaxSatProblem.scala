package pizza

import maxsat._
import org.slf4j.{Logger, LoggerFactory}
import util.rectangle
import util.MapUtils.LoggingParallelMap

object pizzaProblemToMaxSatProblem {
  private implicit val logger: Logger = LoggerFactory.getLogger(getClass)
  private def PizzaClause(negative: Set[Atom[PizzaAtom]], positive: Set[Atom[PizzaAtom]]): Clause[PizzaAtom] =
    Clause(negative, positive)

  private def SliceChosenAtom(slice: Slice): Atom[PizzaAtom] = Atom(SliceChosen(slice))

  private def CellBelongsAtom(cell: Cell): Atom[PizzaAtom] = Atom(CellBelongs(cell.x, cell.y))

  def apply(implicit data: PizzaProblemData): MaxSatProblem[PizzaAtom] = {
    logger.info("Computing 'Cell chosen' definition")
    val cc = cellChosenDefinition
    logger.info("Computing 'non-Overlapping' definition")
    val no = nonOverlappingDefinition

    val problemFormulas = cc ++ no

    logger.info("Computing clause form")
    val hardClauses = problemFormulas.flatMap(f => toClauses(f)).toSet

    logger.info("Computing 'Cell belongs' clauses")
    val cellClauses: Set[Clause[PizzaAtom]] = data.allCells.map(cell =>
      PizzaClause(Set(), Set(CellBelongsAtom(cell)))).toSet

    MaxSatProblem(hardClauses, cellClauses)
  }

  private def cellChosenDefinition(implicit data: PizzaProblemData): Seq[Formula[PizzaAtom]] = {
    data.allCells.par.mapAndLogProgress(cell => {
      Imp(CellBelongsAtom(cell), Or(slicesContaining(cell).map(slice => SliceChosenAtom(slice))))
    }).seq
  }

  private def slicesContaining(cell: Cell)(implicit data: PizzaProblemData): Seq[Slice] = {
    rectangle(Math.max(cell.x - data.problem.maxCells - 1, 0) to cell.x, Math.max(cell.y - data.problem.maxCells - 1, 0) to cell.y).flatMap {
      case (x, y) =>
        val minimumLength = cell.x - x + 1
        val minimumHeight = cell.y - y + 1
        allSlicesAt(x, y)(data.problem).filter(slice => slice.length >= minimumLength && slice.height >= minimumHeight)
    }
  }

  private def nonOverlappingDefinition(implicit data: PizzaProblemData): Seq[Formula[PizzaAtom]] =
    data.allSlices.par.flatMapAndLogProgress(slice1 => computeOverlappingSlices(slice1)
      .map(slice2 => Or(Seq(Not(SliceChosenAtom(slice1)), Not(SliceChosenAtom(slice2)))))).seq

  // Compute those overlapping slices which are to the right and below of the
  // upperLeft of this slice.
  //
  // The other slices will already have been considered (when looping from
  // top-left to bottom-right through all slices.
  private[pizza] def computeOverlappingSlices(slice: Slice)(implicit data: PizzaProblemData): Seq[Slice] = {
    rectangle(slice.upperLeft.x until slice.upperLeft.x + slice.length,
      slice.upperLeft.y until slice.upperLeft.y + slice.height)
      .flatMap { case (x, y) => allSlicesAt(x, y)(data.problem).filterNot(_ == slice) }
  }
}

case class Cell(x: Int, y: Int)

case class Slice(upperLeft: Cell, length: Int, height: Int)

private[pizza] object differentTuples {
  def apply[T](set: Set[T], length: Int): Set[Set[T]] = {
    if (length == 0) Set(Set()) else
      set.flatMap(x => apply(set - x, length - 1).map(tuple => tuple + x))
  }
}