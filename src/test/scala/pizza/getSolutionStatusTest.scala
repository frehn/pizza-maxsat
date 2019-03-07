package pizza

import org.scalatest.{FlatSpec, Matchers}

class getSolutionStatusTest extends FlatSpec with Matchers {
  "isSolutionValid" should "return SolutionValid for a correct solution" in {
    getSolutionStatus(TestData.problemData, TestData.solution) should be(SolutionValid())
  }

  it should "return TooLittleIngredients if there are not enough mushrooms" in {
    getSolutionStatus(TestData.problemData, TestData.noSolutionNoMushrooms) should
      be(TooLittleIngredients(Seq(Slice(Cell(0, 0), 5, 1))))
  }

  it should "return OverlappingSlices if 2 slices overlap" in {
    getSolutionStatus(TestData.problemData, TestData.noSolutionOverlappingSlices) should
      be(OverlappingSlices(Seq((Slice(Cell(0, 0), 2, 3),
        Slice(Cell(0, 0), 3, 1)))))
  }

  it should "return SlicesTooLarge if a slice is too large" in {
    getSolutionStatus(TestData.problemData, TestData.noSolutionSliceTooLarge) should
      be(SlicesTooLarge(Seq(Slice(Cell(0, 1), 5, 2))))
  }

  it should "return NonExistentSlices if a chosen slice is not fully on the pizza" in {
    getSolutionStatus(TestData.problemData, TestData.noSolutionNonExistentSlices) should
      be(NonExistentSlices(Seq(Slice(Cell(2, 1), 2, 3))))
  }
}
