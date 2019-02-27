package pizza

import org.scalatest._

class pizzaProblemToMaxSatProblemTest extends FlatSpec with Matchers {
  "allSlices" should "compute correct slices for a very small problem" in {
    val problem = PizzaProblem(R = 2, C = 3, L = 3, H = 4, (_, _) => Tomato())

    val expectedSlices = Set(
      Slice(Cell(0, 0), 3, 1),
      Slice(Cell(0, 1), 3, 1),
      Slice(Cell(0, 0), 2, 2),
      Slice(Cell(1, 0), 2, 2)
    )

    pizzaProblemToMaxSatProblem.allSlices(problem).toSet should equal(expectedSlices)
  }

  "allSlices" should "compute correct slices for the example problem" in {
    val problem = testProblem
    val slices = pizzaProblemToMaxSatProblem.allSlices(problem).toSet

    System.out.println("SLICES")
    System.out.println(slices)

    slices should contain(Slice(Cell(1, 1), 1, 1))
    slices should not contain(Slice(Cell(2,3),1,4))

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

  "differentTuples" should "compute the n-element subsets of a small set" in {
    val set = Set(1, 2, 3)
    val resultTuples = differentTuples(set, 2)

    val expectedTuples = Set(Set(1, 2), Set(1, 3), Set(2, 3))

    all(resultTuples) should have size 2

    resultTuples.map(_.toSet).toSet should equal(expectedTuples)
  }

  "differentTuples" should "compute the n-element subsets of a bigger set" in {
    val set = Set(1, 2, 3, 4, 5)
    val resultTuples = differentTuples(set, 3)

    resultTuples should have size binomi(set.size, 3)
  }
}

object binomi {
  def apply(n: Int, k: Int): Long = {
    if ((n == k) || (k == 0)) {
      return 1
    }
    else {
      return binomi(n - 1, k) + binomi(n - 1, k - 1)
    }
  }
}
