package pizza

import akka.NotUsed
import akka.stream.scaladsl.Source
import maxsat._
import util.rectangle

object pizzaProblemToMaxSatProblem {
  private def PizzaClause(negative: Set[Atom[PizzaAtom]], positive: Set[Atom[PizzaAtom]]): Clause[PizzaAtom] =
    Clause(negative, positive)

  private def SliceChosenAtom(slice: Slice): Atom[PizzaAtom] = Atom(SliceChosen(slice))

  private def CellBelongsAtom(cell: Cell): Atom[PizzaAtom] = Atom(CellBelongs(cell.x, cell.y))

  def apply(implicit data: PizzaProblemData): MaxSatProblem[PizzaAtom] = {
    val cc = cellChosenDefinition
    val no = nonOverlappingDefinition

    val problemFormulas = cc.concat(no)
    val hardClauses = problemFormulas.flatMapMerge(4, f => toClauses(f)).async

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
    Source(data.allSlices).flatMapMerge(4, slice1 => Source(computeOverlappingSlices(slice1)).async
      .map(slice2 => Or(Seq(Not(SliceChosenAtom(slice1)), Not(SliceChosenAtom(slice2))))))

  // TODO: Maybe possible to optimize by using a specialized allSlicesAt considering minimum
  // length/width of slice.
  private[pizza] def computeOverlappingSlices(slice: Slice)(implicit data: PizzaProblemData): List[Slice] = {
    rectangle(slice.upperLeft.x - data.problem.maxCells to slice.upperLeft.x + slice.length,
      slice.upperLeft.y - data.problem.maxCells to slice.upperLeft.y + slice.height)
      .flatMap { case (x, y) => allSlicesAt(x, y)(data.problem)
        .filter(slice2 => slice2 != slice && doSlicesOverlap(slice2, slice))
      }
      .toList
  }
}

case class Cell(x: Int, y: Int)

case class Slice(upperLeft: Cell, length: Int, height: Int)