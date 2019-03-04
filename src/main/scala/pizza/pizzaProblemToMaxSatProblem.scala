package pizza

import maxsat._
import util.rectangle

object pizzaProblemToMaxSatProblem {

  private def PizzaClause(negative: Set[Atom[PizzaAtom]], positive: Set[Atom[PizzaAtom]]): Clause[PizzaAtom] =
    Clause(negative, positive)

  private def SliceChosenAtom(slice: Slice): Atom[PizzaAtom] = Atom(SliceChosen(slice))

  private def TomatoAtom(cell: Cell): Atom[PizzaAtom] = Atom(TomatoAt(cell.x, cell.y))

  private def MushroomAtom(cell: Cell): Atom[PizzaAtom] = Atom(MushroomAt(cell.x, cell.y))

  private def CellBelongsAtom(cell: Cell): Atom[PizzaAtom] = Atom(CellBelongs(cell.x, cell.y))

  def apply(implicit data: PizzaProblemData): MaxSatProblem[PizzaAtom] = {
    System.out.println("Computing 'Pizza' definition")
    val pd = pizzaDefinition
    System.out.println("Computing 'Cell chosen' definition")
    val cc = cellChosenDefinition
    System.out.println("Computing 'Is valid' definition")
    val v = isValidDefinition

    val problemFormulas = Seq(pd, cc, v)

    System.out.println("Computing clause form")
    val hardClauses = problemFormulas.flatMap(f => toClauses(f)).toSet
    System.out.println("Computed clause form")

    System.out.println("Computing 'Cell belongs' clauses")
    val cellClauses: Set[Clause[PizzaAtom]] = data.allCells.map(cell =>
      PizzaClause(Set(), Set(CellBelongsAtom(cell)))).toSet

    MaxSatProblem(hardClauses, cellClauses)
  }

  private def cellChosenDefinition(implicit data: PizzaProblemData): Formula[PizzaAtom] = {
    And(data.allCells.map(cell => {
      Imp(CellBelongsAtom(cell), Or(data.allSlices.filter(slice => cellContainedIn(cell, slice)).map(slice =>
        SliceChosenAtom(slice)
      )))
    }))
  }

  private def isValidDefinition(implicit data: PizzaProblemData): Formula[PizzaAtom] =
    And(data.allSlices.map(slice => Imp(Atom(SliceChosen(slice)), validSlice(slice))) ++
      data.allSlices.map(slice =>
        Imp(SliceChosenAtom(slice),
          And(computeOverlappingSlices(slice).map(slice2 => Not(SliceChosenAtom(slice2))))))
    )

  // Compute those overlapping slices which are to the right and below of the
  // upperLeft of this slice.
  //
  // The other slices will already have been considered (when looping from
  // top-left to bottom-right through all slices.
  private[pizza] def computeOverlappingSlices(slice: Slice)(implicit data: PizzaProblemData): Seq[Slice] = {
    rectangle(slice.upperLeft.x until slice.upperLeft.x + slice.length,
      slice.upperLeft.y until slice.upperLeft.y + slice.width)
      .flatMap { case (x, y) => allSlicesAt(x, y, data.problem).filterNot(_ == slice) }
  }

  private def pizzaDefinition(implicit data: PizzaProblemData): Formula[PizzaAtom] = {
    And((0 until data.problem.C).flatMap(i => {
      (0 until data.problem.R).map(j => {
        val c = Cell(i, j)
        data.problem.ingredient(i, j) match {
          case Tomato() => And(Seq(TomatoAtom(c), Not(MushroomAtom(c))))
          case Mushroom() => And(Seq(MushroomAtom(c), Not(TomatoAtom(c))))
        }
      })
    }))
  }

  private def validSlice(slice: Slice)(implicit data: PizzaProblemData): Formula[PizzaAtom] = {
    And(Seq(validIngredient(slice, Tomato()), validIngredient(slice, Mushroom())))
  }

  private def validIngredient(slice: Slice, ingredient: Ingredient)(implicit pp: PizzaProblemData): Formula[PizzaAtom] = {
    Or(differentCellTuples(slice).toSeq.map(cells =>
      And(cells.toSeq.map(cell => ingredient match {
        case Tomato() => TomatoAtom(cell)
        case Mushroom() => MushroomAtom(cell)
      }))
    ))
  }

  // compute L-tuples of cells of the slice that are pairwise different
  private def differentCellTuples(slice: Slice)(implicit data: PizzaProblemData): Set[Set[Cell]] = {
    differentTuples(cells(slice).toSet, data.problem.L)
  }

  private[pizza] def cells(slice: Slice): Seq[Cell] = (slice.upperLeft.x until slice.upperLeft.x + slice.length).flatMap(x =>
    (slice.upperLeft.y until slice.upperLeft.y + slice.width).map(y => Cell(x, y))
  )
}

case class Cell(x: Int, y: Int)

case class Slice(upperLeft: Cell, length: Int, width: Int)

private[pizza] object differentTuples {
  /*  @tailrec
    def apply[T](set: Set[T], length: Int, current: Set[Set[T]] = Set(Set[T]())): Set[Set[T]] = {
      val finished = current.filter(_.size == length)
      val notFinished = current.filter(_.size < length)

      if (notFinished.isEmpty)
        current
      else {
        val newCurrent = set.flatMap(x => {
          notFinished.map(tuple =>
            tuple + x
          )
        }) ++ finished

        apply(set, length, newCurrent)
      }
    }
  */
  def apply[T](set: Set[T], length: Int): Set[Set[T]] = {
    if (length == 0) Set(Set()) else
      set.flatMap(x => apply(set - x, length - 1).map(tuple => tuple + x))
  }
}