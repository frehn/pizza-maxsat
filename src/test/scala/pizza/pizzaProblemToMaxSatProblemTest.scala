package pizza

import org.scalatest._
import pizza.parser.parsePizzaProblem

class pizzaProblemToMaxSatProblemTest extends FlatSpec with Matchers {
  "computeOverlappingSlices" should "compute the overlapping slices (those that are to the right and below)" in {
    val slice = Slice(Cell(0, 1), 2, 2)
    val overlappingSlices = pizzaProblemToMaxSatProblem.computeOverlappingSlices(slice)(TestData.problemData)

    overlappingSlices should contain(Slice(Cell(1, 1), 2, 2))
  }

  it should "compute an example correctly (1)" in {
    val b_small = parsePizzaProblem(scala.io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("b_small.in")).mkString)
    val data = pizzaProblemToData(b_small)
    val slice1 = Slice(Cell(5, 1), 2, 2)
    val slice2 = Slice(Cell(1, 2), 5, 1)
    val overlappingSlices = pizzaProblemToMaxSatProblem.computeOverlappingSlices(slice1)(data)

    overlappingSlices should contain(slice2)
  }
}

object binomi {
  def apply(n: Int, k: Int): Long = {
    if ((n == k) || (k == 0))
      1
    else
      binomi(n - 1, k) + binomi(n - 1, k - 1)
  }
}
