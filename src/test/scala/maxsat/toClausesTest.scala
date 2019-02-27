package maxsat

import org.scalatest.{FlatSpec, Matchers}

class toClausesTest extends FlatSpec with Matchers {
  "toCNF" should "convert a formula to CNF (1)" in {
    val a = Atom[Int](1)
    val b = Atom[Int](2)
    val c = Atom[Int](3)
    val d = Atom[Int](4)

    val formula = Or(Seq(And(Seq(a, b)), And(Seq(c, d))))

    val expectedClauses = Set(
      Clause(negative = Set[Atom[Int]](), positive = Set(a, c)),
      Clause(negative = Set[Atom[Int]](), positive = Set(a, d)),
      Clause(negative = Set[Atom[Int]](), positive = Set(b, c)),
      Clause(negative = Set[Atom[Int]](), positive = Set(b, d))
    )

    toClauses(formula) should equal(expectedClauses)
  }

  "toCNF" should "convert a formula to CNF (2)" in {
    val a = Atom[Int](1)
    val b = Atom[Int](2)
    val c = Atom[Int](3)
    val d = Atom[Int](4)
    val e = Atom[Int](5)
    val f = Atom[Int](6)

    val formula = Or(Seq(And(Seq(a, b)), And(Seq(c, d)), And(Seq(e, f))))

    val expectedClauses = Set(
      Clause(negative = Set[Atom[Int]](), positive = Set(a, c, e)),
      Clause(negative = Set[Atom[Int]](), positive = Set(a, d, e)),
      Clause(negative = Set[Atom[Int]](), positive = Set(a, c, f)),
      Clause(negative = Set[Atom[Int]](), positive = Set(a, d, f)),
      Clause(negative = Set[Atom[Int]](), positive = Set(b, c, e)),
      Clause(negative = Set[Atom[Int]](), positive = Set(b, d, e)),
      Clause(negative = Set[Atom[Int]](), positive = Set(b, c, f)),
      Clause(negative = Set[Atom[Int]](), positive = Set(b, d, f)),
    )

    toClauses(formula) should equal(expectedClauses)
  }
}