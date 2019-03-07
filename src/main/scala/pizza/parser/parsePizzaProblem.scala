package pizza.parser

import pizza.{Ingredient, Mushroom, PizzaProblem, Tomato}

object parsePizzaProblem {
  def apply(input: String): PizzaProblem = {
    val lines = input.split("\n")
    val tokens = lines(0).split(" ")

    val map: Map[(Int, Int), Ingredient] = lines.tail.zipWithIndex.flatMap { case (line, j) =>
      line.split("").zipWithIndex.map { case (char, i) =>
        (i, j) -> (if (char == "T")
          Tomato()
        else
          Mushroom())
      }
    }.toMap

    PizzaProblem(row = tokens(0).toInt,
      columns = tokens(1).toInt,
      minIngredients = tokens(2).toInt,
      maxCells = tokens(3).toInt,
      map)
  }
}
