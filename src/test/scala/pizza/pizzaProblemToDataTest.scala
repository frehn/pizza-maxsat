package pizza

import org.scalatest.{FlatSpec, Matchers}
import pizza.TestData.problem
import util.rectangle

class pizzaProblemToDataTest extends FlatSpec with Matchers {
  "pizzaProblemToData" should "compute correct slices for a very small problem" in {
    val simpleProblem = {
      val R = 2
      val C = 3
      val L = 3
      val H = 4

      val ingredients = rectangle(0 until R, 0 until C)
        .map { case (x, y) => (x, y) -> Tomato() }
        .toMap

      PizzaProblem(R, C, L, H, ingredients)
    }

    val expectedSlices = Set(
      Slice(Cell(0, 0), 3, 1),
      Slice(Cell(0, 1), 3, 1),
      Slice(Cell(0, 0), 2, 2),
      Slice(Cell(1, 0), 2, 2)
    )

    pizzaProblemToData.allSlices(simpleProblem).toSet should equal(expectedSlices)
  }

  it should "compute correct slices for the example problem" in {
    val slices = pizzaProblemToData.allSlices(problem).toSet

    System.out.println("SLICES")
    System.out.println(slices)

    slices should contain(Slice(Cell(1, 1), 1, 1))
    slices should not contain Slice(Cell(2, 3), 1, 4)

  }


  "cells" should "compute the cells of a slice" in {
    val rows = 3
    val cols = 2
    val slice = Slice(Cell(1, 2), rows, cols)

    val expectedCells = Set(
      Cell(1, 2),
      Cell(2, 2),
      Cell(3, 2),
      Cell(1, 3),
      Cell(2, 3),
      Cell(3, 3)
    )

    val computedCells = pizzaProblemToMaxSatProblem.cells(slice)

    computedCells.length should equal(rows * cols)
    computedCells.toSet should equal(expectedCells)
  }
}
