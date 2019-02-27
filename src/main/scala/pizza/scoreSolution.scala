package pizza

object scoreSolution {
  def apply(solution: PizzaSolution):Int = {
    solution.slices.map(sliceSize).sum
  }

  private def sliceSize(slice: Slice) = slice.length*slice.width
}
