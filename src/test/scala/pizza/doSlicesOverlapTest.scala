package pizza

import org.scalatest.{FlatSpec, Matchers}

class doSlicesOverlapTest extends FlatSpec with Matchers {
  "doSlicesOverlap" should "detect an overlap simple examples (1)" in {
    doSlicesOverlap(Slice(Cell(1, 1), 1, 2), Slice(Cell(1, 1), 1, 1)) should be(true)
  }

  it should "detect an overlap simple examples (2)" in {
    doSlicesOverlap(Slice(Cell(0, 0), 1, 3), Slice(Cell(0, 1), 3, 2)) should be(true)
  }

  it should "detect an overlap simple examples (3)" in {
    doSlicesOverlap(Slice(Cell(1, 0), 2, 3), Slice(Cell(0, 1), 3, 2)) should be(true)
  }

  it should "detect a non-overlap for some simple examples" in {
    doSlicesOverlap(Slice(Cell(1, 1), 1, 2), Slice(Cell(2, 1), 1, 1)) should be(false)
    doSlicesOverlap(Slice(Cell(2, 4), 5, 1), Slice(Cell(2, 5), 5, 1)) should be(false)
  }
}
