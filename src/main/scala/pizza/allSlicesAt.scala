package pizza

object allSlicesAt {
  def apply(x: Int, y: Int)(implicit problem: PizzaProblem): Seq[Slice] =
    (1 to problem.maxCells).flatMap(l => {
      (ceilDivision(problem.minIngredients, l) to floorDivision(problem.maxCells, l)).flatMap(w => {
        val slice = Slice(Cell(x, y), l, w)
        if (x + l - 1 < problem.columns && y + w - 1 < problem.rows && hasEnoughIngredients(slice))
          Seq(slice)
        else
          Seq()
      })
    })

  private def floorDivision(a: Int, b: Int): Int = a / b

  private def ceilDivision(a: Int, b: Int): Int = (a - 1) / b + 1
}
