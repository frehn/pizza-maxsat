package pizza

import util.rectangle

object pizzaProblemToData {
  def apply(pp: PizzaProblem): PizzaProblemData = PizzaProblemData(allSlices(pp), allCells(pp), pp)

  private[pizza] def allSlices(implicit pp: PizzaProblem): Seq[Slice] = {
    System.out.println("Computing all slices with enough ingredients")
    val ret = rectangle(0 until pp.columns, 0 until pp.rows).flatMap { case (i, j) =>
      allSlicesAt(i, j)(pp)
    }
    System.out.println(s"Computed all ${ret.size} valid slices")
    ret
  }

  private[pizza] def cells(slice: Slice): Seq[Cell] = (slice.upperLeft.x until slice.upperLeft.x + slice.length).flatMap(x =>
    (slice.upperLeft.y until slice.upperLeft.y + slice.height).map(y => Cell(x, y))
  )

  private def allCells(pp: PizzaProblem): Seq[Cell] =
    rectangle(0 until pp.columns, 0 until pp.rows).map { case (i, j) =>
      Cell(i, j)
    }
}
