package pizza

import maxsat._

object pizzaProblemToMaxSatProblem {

  private def PizzaClause(negative: Set[Atom[PizzaAtom]], positive: Set[Atom[PizzaAtom]]): Clause[PizzaAtom] =
    Clause(negative, positive)

  private def SliceChosenAtom(slice: Slice): Atom[PizzaAtom] = Atom(SliceChosen(slice))

  private def TomatoAtom(cell: Cell): Atom[PizzaAtom] = Atom(TomatoAt(cell.x, cell.y))

  private def MushroomAtom(cell: Cell): Atom[PizzaAtom] = Atom(MushroomAt(cell.x, cell.y))

  private def CellBelongsAtom(cell: Cell): Atom[PizzaAtom] = Atom(CellBelongs(cell.x, cell.y))

  def apply(pp: PizzaProblem): MaxSatProblem[PizzaAtom] = {
    implicit val data = PizzaProblemData(pp, allSlices(pp))
    System.out.println("Computing formulas")
    val problemFormulas = data.allSlices.map(sliceChosenDefinition) :+ isValid
    val sliceClauses: Set[Clause[PizzaAtom]] = data.allSlices.map(slice =>
      PizzaClause(Set(), Set(SliceChosenAtom(slice)))).toSet
    System.out.println("Computed formulas")

    System.out.println("Computing clause form")
    val hardClauses = problemFormulas.flatMap(f => toClauses(f)).toSet
    System.out.println("Computed clause form")
    MaxSatProblem(hardClauses, sliceClauses)
  }

  private def sliceChosenDefinition(slice: Slice): Formula[PizzaAtom] = {
    Eq(SliceChosenAtom(slice), And(cells(slice).map(cell => CellBelongsAtom(cell))))
  }

  private def isValid(implicit data: PizzaProblemData): Formula[PizzaAtom] = {
    And(Seq(
      And(data.allSlices.map(slice => Imp(Atom(SliceChosen(slice)), validSlice(slice)))),
      And((0 until data.allSlices.length).flatMap(i => {
        (i + 1 until data.allSlices.length).flatMap(j => {
          val slice1 = data.allSlices.apply(i)
          val slice2 = data.allSlices.apply(j)
          // TODO: optimization? compute overlapping slices for slice1
          if (doSlicesOverlap(slice1, slice2)) {
            Seq(Or[PizzaAtom](Seq(Not(SliceChosenAtom(slice1)), Not(SliceChosenAtom(slice2)))))
          } else
            Seq()
        })
      }
      ))
    ))
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
    differentTuples.apply(cells(slice).toSet, data.pp.L)
  }


  private[pizza] def allSlices(pp: PizzaProblem): Seq[Slice] = {
    System.out.println("Computing all slices")
    val ret = (0 to pp.C).flatMap(i => {
      (0 to pp.R).flatMap(j => {
        (1 to pp.H).flatMap(l => {
          (ceilDivision(pp.L, l) to floorDivision(pp.H, l)).flatMap(w => {
            if (i + l - 1 < pp.C && j + w - 1 < pp.R)
              Seq(Slice(Cell(i, j), l, w))
            else
              Seq()
          })
        })
      })
    })
    System.out.println(s"Computed all ${ret.size} slices")
    ret
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