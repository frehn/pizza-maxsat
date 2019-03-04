package pizza

import util.rectangle

object pizzaProblemToData {
  def apply(pp: PizzaProblem): PizzaProblemData = PizzaProblemData(pp, allSlices(pp), allCells(pp))

  private[pizza] def allSlices(pp: PizzaProblem): Seq[Slice] = {
    System.out.println("Computing all slices")
    val ret = rectangle(0 until pp.C, 0 until pp.R).flatMap { case (i, j) =>
      allSlicesAt(i, j, pp)
    }
    System.out.println(s"Computed all ${ret.size} slices")
    ret
  }

  private def allCells(pp: PizzaProblem): Seq[Cell] =
    rectangle(0 until pp.C, 0 until pp.R).map { case (i, j) =>
      Cell(i, j)
    }
}
