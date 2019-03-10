package pizza

import org.scalatest.{FlatSpec, Matchers}
import pizza.TestData.problem
import util.rectangle
import org.scalatest.Inspectors._

class pizzaProblemToDataTest extends FlatSpec with Matchers {
  "pizzaProblemToData" should "compute all valid slices for a very small problem" in {
    val simpleProblem = {
      val rows = 2
      val columns = 3
      val minIngredients = 2
      val maxCells = 4

      val ingredients = rectangle(0 until columns, 0 until rows)
        .map { case (x, y) => (x, y) -> (if (y == 0) Tomato() else Mushroom()) }
        .toMap

      PizzaProblem(rows, columns, minIngredients, maxCells, ingredients)
    }

    val expectedSlices = Set(
      Slice(Cell(0, 0), 2, 2),
      Slice(Cell(1, 0), 2, 2)
    )

    pizzaProblemToData.allSlices(simpleProblem).toSet should equal(expectedSlices)
  }

  it should "have no slices with x, y < 0" in {
    val slices = pizzaProblemToData(problem).allSlices

    all(slices.map(slice => slice.upperLeft.x)) should be >= 0
    all(slices.map(slice => slice.upperLeft.y)) should be >= 0
  }

  it should "compute valid slices for the example problem" in {
    val slices = pizzaProblemToData.allSlices(problem).toSet

    val expectedSlices = Set(
      Slice(Cell(1, 0), 1, 3),
      Slice(Cell(2, 0), 1, 3),
      Slice(Cell(3, 0), 1, 3),
      Slice(Cell(1, 0), 2, 3),
      Slice(Cell(2, 0), 2, 3),
      Slice(Cell(3, 0), 2, 3),
      Slice(Cell(1, 0), 1, 2),
      Slice(Cell(0, 1), 2, 1)
    )

    forAll (expectedSlices) {
      slice => slices should contain(slice)
    }

    slices should not contain Slice(Cell(2, 3), 1, 4)
    slices should not contain Slice(Cell(0, 0), 4, 1)
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

    val computedCells = pizzaProblemToData.cells(slice)

    computedCells.length should equal(rows * cols)
    computedCells.toSet should equal(expectedCells)
  }
}
