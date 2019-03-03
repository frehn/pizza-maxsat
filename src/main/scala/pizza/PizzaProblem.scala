package pizza

case class PizzaProblem(R: Int, C: Int, L: Int, H: Int, ingredient: Map[(Int, Int) /* cell */, Ingredient])

sealed trait Ingredient

case class Mushroom() extends Ingredient
case class Tomato() extends Ingredient