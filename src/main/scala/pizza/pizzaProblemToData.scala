package pizza

import util.rectangle

object pizzaProblemToData {
  def apply(pp: PizzaProblem): PizzaProblemData = PizzaProblemData(pp, allSlices(pp), allCells(pp))

  private[pizza] def allSlices(pp: PizzaProblem): Seq[Slice] = {
    System.out.println("Computing all slices")
    val ret = rectangle(0 until pp.C, 0 until pp.R).flatMap { case (i, j) =>
      (1 to pp.H).flatMap(l => {
        (ceilDivision(pp.L, l) to floorDivision(pp.H, l)).flatMap(w => {
          if (i + l - 1 < pp.C && j + w - 1 < pp.R)
            Seq(Slice(Cell(i, j), l, w))
          else
            Seq()
        })
      })
    }
    System.out.println(s"Computed all ${ret.size} slices")
    ret
  }

  private def allCells(pp: PizzaProblem): Seq[Cell] =
    rectangle(0 until pp.C, 0 until pp.R).map { case (i, j) =>
      Cell(i, j)
    }


  private def floorDivision(a: Int, b: Int): Int = a / b

  private def ceilDivision(a: Int, b: Int): Int = (a - 1) / b + 1
}
