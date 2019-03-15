package pizza

import akka.NotUsed
import akka.stream.scaladsl.Source
import maxsat._
import org.slf4j.{Logger, LoggerFactory}
import util.rectangle

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

    val problemFormulas = cc.concat(no)

    logger.info("Computing clause form")
    val hardClauses = problemFormulas.flatMapConcat(f => toClauses(f))

    logger.info("Computing 'Cell belongs' clauses")
    val cellClauses: Set[Clause[PizzaAtom]] = data.allCells.map(cell =>
      PizzaClause(Set(), Set(CellBelongsAtom(cell)))).toSet

    MaxSatProblem(hardClauses, cellClauses)
  }

  private def cellChosenDefinition(implicit data: PizzaProblemData): Source[Formula[PizzaAtom], NotUsed] = {
    Source(data.allCells).map(cell =>
      Imp(CellBelongsAtom(cell), Or(slicesContaining(cell).map(slice => SliceChosenAtom(slice)))))
  }

  private def slicesContaining(cell: Cell)(implicit data: PizzaProblemData): Seq[Slice] = {
    rectangle(Math.max(cell.x - data.problem.maxCells - 1, 0) to cell.x, Math.max(cell.y - data.problem.maxCells - 1, 0) to cell.y).flatMap {
      case (x, y) =>
        val minimumLength = cell.x - x + 1
        val minimumHeight = cell.y - y + 1
        allSlicesAt(x, y)(data.problem).filter(slice => slice.length >= minimumLength && slice.height >= minimumHeight)
    }
  }

  private def nonOverlappingDefinition(implicit data: PizzaProblemData): Source[Formula[PizzaAtom], NotUsed] =
    Source(data.allSlices).flatMapConcat(slice1 => Source(computeOverlappingSlices(slice1))
      .map(slice2 => Or(Seq(Not(SliceChosenAtom(slice1)), Not(SliceChosenAtom(slice2))))))


  // TODO: Maybe possible to optimize by using a specialized allSlicesAt
  private[pizza] def computeOverlappingSlices(slice: Slice)(implicit data: PizzaProblemData): List[Slice] = {
    rectangle(slice.upperLeft.x-data.problem.maxCells to slice.upperLeft.x + slice.length,
      slice.upperLeft.y-data.problem.maxCells to slice.upperLeft.y + slice.height)
      .flatMap{ case (x,y) => allSlicesAt(x, y)(data.problem)
      .filter( slice2 => slice2 != slice && doSlicesOverlap(slice2, slice))
      }
      .toList
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