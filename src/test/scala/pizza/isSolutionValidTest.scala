package pizza

import org.scalatest.{FlatSpec, Matchers}

class isSolutionValidTest extends FlatSpec with Matchers {
  "isSolutionValid" should "work on example data" in {
    val data = pizzaProblemToData(TestData.problem)
    isSolutionValid(data, TestData.solution) should be(true)
    isSolutionValid(data, TestData.noSolutionNoMushrooms) should be(false)
    isSolutionValid(data, TestData.noSolutionOverlap) should be(false)
    isSolutionValid(data, TestData.noSolutionSliceTooLarge) should be(false)
    isSolutionValid(data, TestData.noSolutionSliceDoesNotExist) should be(false)
  }

}
