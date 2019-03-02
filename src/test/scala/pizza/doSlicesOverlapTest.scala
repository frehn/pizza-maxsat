package pizza

import org.scalatest.{FlatSpec, Matchers}

class doSlicesOverlapTest extends FlatSpec with Matchers {
  "doSlicesOverlap" should "detect an overlap for some simple examples" in {
    doSlicesOverlap(Slice(Cell(1, 1), 1, 2), Slice(Cell(1, 1), 1, 1)) should be(true)
    doSlicesOverlap(Slice(Cell(0, 0), 1, 3), Slice(Cell(0, 1), 3, 2)) should be(true)
    doSlicesOverlap(Slice(Cell(1, 0), 2, 3), Slice(Cell(0, 1), 3, 2)) should be(true)
  }

  "doSlicesOverlap" should "detect a non-overlap for some simple examples" in {
    doSlicesOverlap(Slice(Cell(1, 1), 1, 2), Slice(Cell(2, 1), 1, 1)) should be(false)
  }
}
