package pizza

case class PizzaProblem(rows: Int, columns: Int, minIngredients: Int, maxCells: Int, ingredient: Map[(Int, Int) /* cell */, Ingredient])

sealed trait Ingredient

case class Mushroom() extends Ingredient
case class Tomato() extends Ingredient