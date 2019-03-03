package util

import org.scalatest.{FlatSpec, Matchers}

class rectangleTest extends FlatSpec with Matchers {
  "rectangle" should "construct a sequences of pairs representing a rectangle" in {
    val xs = Seq(1, 2, 3)
    val ys = Seq("A", "B")

    val expected = Set((1, "A"), (1, "B"), (2, "A"), (2, "B"), (3, "A"), (3, "B"))

    rectangle(xs, ys).toSet should equal(expected)
  }
}
