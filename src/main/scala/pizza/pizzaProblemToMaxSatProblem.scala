package pizza

import maxsat._

object pizzaProblemToMaxSatProblem {
  def apply(pp: PizzaProblem): MaxSatProblem[PizzaAtom] = {
    implicit val data = PizzaProblemData(pp, allSlices(pp))
    val problemFormulas = data.allSlices.map(sliceChosenDefinition) :+ isValid
    val sliceClauses: Set[Clause[PizzaAtom]] = data.allSlices.map(slice => Clause[PizzaAtom](Set[Atom[PizzaAtom]](), Set(Atom[PizzaAtom](SliceChosen(slice))))).toSet

    MaxSatProblem(problemFormulas.flatMap(f => toClauses(f)).toSet, sliceClauses)
  }

  private def validSlice(slice: Slice)(implicit data: PizzaProblemData): Formula[PizzaAtom] = {
    And(Seq(validIngredient(slice, Tomato()), validIngredient(slice, Mushroom())))
  }

  private def validIngredient(slice: Slice, ingredient: Ingredient)(implicit pp: PizzaProblemData): Formula[PizzaAtom] = {
    Or(differentCellTuples(slice).toSeq.map(cells =>
      And(cells.toSeq.map(cell => ingredient match {
        case Tomato() => Atom[PizzaAtom](TomatoAt(cell.x, cell.y))
        case Mushroom() => Atom[PizzaAtom](MushroomAt(cell.x, cell.y))
      }))
    ))
  }

  // compute L-tuples of cells of the slice that are pairwise different
  private def differentCellTuples(slice: Slice)(implicit data: PizzaProblemData): Set[Set[Cell]] = {
    differentTuples.apply(cells(slice).toSet, data.pp.L)
  }

  private def isValid(implicit data: PizzaProblemData): Formula[PizzaAtom] = {
    And(Seq(
      And(data.allSlices.map(slice => Imp(Atom(SliceChosen(slice)), validSlice(slice)))),
      And[PizzaAtom]((0 until data.allSlices.length).flatMap(i => {
        (i + 1 until data.allSlices.length).flatMap(j => {
          val slice1 = data.allSlices.apply(i)
          val slice2 = data.allSlices.apply(j)
          // TODO: optimization? compute overlapping slices for slice1
          if (overlapping(slice1, slice2)) {
            Seq(Or[PizzaAtom](Seq(Not(Atom(SliceChosen(slice1))), Not(Atom(SliceChosen(slice2))))))
          } else
            Seq()
        })
      }
      ))
    ))
  }

  private def sliceChosenDefinition(slice: Slice): Formula[PizzaAtom] = {
    Eq(Atom(SliceChosen(slice)), And(cells(slice).map(cell => Atom[PizzaAtom](CellBelongs(cell.x, cell.y)))))
  }

  private def overlapping(slice1: Slice, slice2: Slice): Boolean = {
    def check(x1: Int, x2: Int, length1: Int, length2: Int): Boolean =
      if (x1 <= x2) {
        x1 + length1 <= x2
      } else {
        x2 + length2 <= x1
      }

    check(slice1.upperLeft.x, slice2.upperLeft.x, slice1.length, slice2.length) ||
      check(slice1.upperLeft.y, slice1.upperLeft.y, slice1.width, slice2.width)
  }

  private[pizza] def allSlices(pp: PizzaProblem): Seq[Slice] = {
    (1 to pp.R).flatMap(i => {
      (1 to pp.C).flatMap(j => {
        (1 to pp.H).flatMap(l => {
          (ceilDivision(pp.L, l) to floorDivision(pp.H, l)).flatMap(w => {
            if (i + l - 1 <= pp.R && j + w - 1 <= pp.C)
              Seq(Slice(Cell(j, i), l, w))
            else
              Seq()
          })
        })
      })
    })
  }

  private[pizza] def cells(slice: Slice): Seq[Cell] = {
    (slice.upperLeft.x to slice.upperLeft.x + slice.length - 1).flatMap(x =>
      (slice.upperLeft.y to slice.upperLeft.y + slice.width - 1).map(y => Cell(x, y))
    )
  }

  private def floorDivision(a: Int, b: Int): Int = a / b

  private def ceilDivision(a: Int, b: Int): Int = (a - 1) / b + 1
}

case class PizzaProblemData(pp: PizzaProblem, allSlices: Seq[Slice])

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