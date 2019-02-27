package pizza.parser

import pizza.{Ingredient, Mushroom, PizzaProblem, Tomato}

object parsePizzaProblem {
  def apply(lines: Seq[String]) = {
    val tokens = lines(0).split(" ")

    val map : Map[(Int, Int), Ingredient] = lines.tail.zipWithIndex.flatMap{ case (line, i) => {
      line.split("").zipWithIndex.map{ case (char, j) => {
        (i, j) -> (if (char == "T")
          Tomato()
        else
          Mushroom())
      }}
    }}.toMap

    PizzaProblem(R = tokens(0).toInt,
      C = tokens(1).toInt,
      L = tokens(2).toInt ,
      H = tokens(3).toInt,
      (x, y) => { map((x,y)) } )
  }
}
