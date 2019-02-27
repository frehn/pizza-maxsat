package pizza

case class PizzaProblem(R: Int, C: Int, L: Int, H: Int, ingredient: (Int, Int) => Ingredient)

sealed trait Ingredient

case class Mushroom() extends Ingredient
case class Tomato() extends Ingredient