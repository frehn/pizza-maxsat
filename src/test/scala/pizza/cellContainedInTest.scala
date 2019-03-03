package pizza

import org.scalatest.{FlatSpec, Matchers}

class cellContainedInTest extends FlatSpec with Matchers {
  val slice = Slice(Cell(1, 2), 3, 2)

  "cellContainedIn" should "return true for its top-left cell" in {
    cellContainedIn(slice.upperLeft, slice) should be(true)
  }

  "cellContainedIn" should "return true for its bottom-right cell" in {
    cellContainedIn(Cell(3, 3), slice) should be(true)
  }

  "cellContainedIn" should "return false for a cell to the right" in {
    cellContainedIn(Cell(4, 2), slice) should be(false)
  }

  "cellContainedIn" should "return false for a cell to the above" in {
    cellContainedIn(Cell(2, 0), slice) should be(false)
  }
}
