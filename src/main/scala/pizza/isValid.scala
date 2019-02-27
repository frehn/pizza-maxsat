package pizza

object isValid {
  def apply(solution: PizzaSolution): Boolean = {
    !solution.slices.zipWithIndex.exists { case (slice1, i) => {
      solution.slices.drop(i + 1).exists(slice2 => {
        doSlicesOverlap(slice1, slice2)
      })
    }
    }
  }
}
