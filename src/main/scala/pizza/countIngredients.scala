package pizza

import util.rectangle

object countIngredients {
  def apply(slice: Slice)(implicit problem: PizzaProblem): IngredientCount =
    apply(slice.upperLeft.x, slice.upperLeft.y, slice.length, slice.height)

  def apply(x: Int, y: Int, length: Int, height: Int)(implicit problem: PizzaProblem): IngredientCount =
    rectangle(0 until length, 0 until height).foldLeft(IngredientCount(0, 0)) { case (result, (i, j)) =>
      problem.ingredient.lift(x + i, y + j) match {
        case Some(Tomato()) => IngredientCount(result.tomatoes + 1, result.mushrooms)
        case Some(Mushroom()) => IngredientCount(result.tomatoes, result.mushrooms + 1)
        case None => IngredientCount(result.tomatoes, result.mushrooms)
      }
    }
}
