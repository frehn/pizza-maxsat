package pizza

object allSlicesAt {
  def apply(x: Int, y: Int, problem: PizzaProblem): Seq[Slice] =
    (1 to problem.H).flatMap(l => {
      (ceilDivision(problem.L, l) to floorDivision(problem.H, l)).flatMap(w => {
        if (x + l - 1 < problem.C && y + w - 1 < problem.R)
          Seq(Slice(Cell(x, y), l, w))
        else
          Seq()
      })
    })

  private def floorDivision(a: Int, b: Int): Int = a / b

  private def ceilDivision(a: Int, b: Int): Int = (a - 1) / b + 1
}
