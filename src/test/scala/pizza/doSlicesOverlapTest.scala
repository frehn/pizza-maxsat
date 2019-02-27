package pizza

import org.scalatest.{FlatSpec, Matchers}

class doSlicesOverlapTest extends FlatSpec with Matchers {
  "doSlicesOverlap" should "detect an overlap for a simple example" in {
    val slice1 = Slice(Cell(1,1),1,2)
    val slice2 = Slice(Cell(1,1),1,1)
    doSlicesOverlap(slice1, slice2) should be(true)
  }

  "doSlicesOverlap" should "detect a non-overlap for a simple example" in {
    val slice1 = Slice(Cell(1,1),1,2)
    val slice2 = Slice(Cell(2,1),1,1)
    doSlicesOverlap(slice1, slice2) should be(false)
  }
}
