package pizza

import org.scalatest.{FlatSpec, Matchers}

class isSolutionValidTest extends FlatSpec with Matchers {
  "isSolutionValid" should "return true for a correct solution" in {
    isSolutionValid(TestData.problemData, TestData.solution) should be(true)
  }

  it should "return false if there are not enough mushrooms" in {
    isSolutionValid(TestData.problemData, TestData.noSolutionNoMushrooms) should be(false)
  }

  it should "return false if 2 slices overlap" in {
    isSolutionValid(TestData.problemData, TestData.noSolutionOverlap) should be(false)
  }

  it should "return false if a slice is too large" in {
    isSolutionValid(TestData.problemData, TestData.noSolutionSliceTooLarge) should be(false)
  }

  it should "return false if a chosen slice is not fully on the pizza" in {
    isSolutionValid(TestData.problemData, TestData.noSolutionSliceDoesNotExist) should be(false)
  }
}
