package pizza

object hasEnoughIngredients {
  def apply(slice: Slice)(implicit problem: PizzaProblem): Boolean =
    countIngredients(slice) match {
      case IngredientCount(tomatoes, mushrooms) => tomatoes >= problem.minIngredients && mushrooms >= problem.minIngredients
    }
}
