package pizza

object hasEnoughIngredients {
  def apply(slice: Slice)(implicit problem: PizzaProblem): Boolean =
    countIngredients(slice) match {
      case IngredientCount(tomatoes, mushrooms) => hasEnough(tomatoes, mushrooms)
    }

  def apply(x: Int, y: Int, length: Int, height: Int)(implicit problem: PizzaProblem): Boolean = {
    countIngredients(x, y, length, height) match {
      case IngredientCount(tomatoes, mushrooms) => hasEnough(tomatoes, mushrooms)
    }
  }

  private def hasEnough(tomatoes: Int, mushrooms: Int)(implicit problem: PizzaProblem) = {
    tomatoes >= problem.minIngredients && mushrooms >= problem.minIngredients
  }

}
