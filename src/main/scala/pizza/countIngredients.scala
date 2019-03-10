package pizza

import util.rectangle

object countIngredients {
  def apply(slice: Slice)(implicit problem: PizzaProblem): IngredientCount = {
    rectangle(0 until slice.length, 0 until slice.height).foldLeft(IngredientCount(0, 0)) { case (result, (i, j)) =>
      problem.ingredient.lift(slice.upperLeft.x + i, slice.upperLeft.y + j) match {
        case Some(Tomato()) => IngredientCount(result.tomatoes + 1, result.mushrooms)
        case Some(Mushroom()) => IngredientCount(result.tomatoes, result.mushrooms + 1)
        case None => IngredientCount(result.tomatoes, result.mushrooms)
      }
    }
  }
}
