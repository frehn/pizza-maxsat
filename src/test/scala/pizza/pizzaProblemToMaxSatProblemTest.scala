package pizza

import org.scalatest._
import pizza.TestData.problem
import util.rectangle

class pizzaProblemToMaxSatProblemTest extends FlatSpec with Matchers {
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
