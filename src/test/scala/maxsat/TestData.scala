package maxsat

object TestData {
  private val a = Atom[Int](1)
  private val b = Atom[Int](2)
  private val c = Atom[Int](3)

  val testClauses = Set(Clause(Set(a), Set(b)), Clause(Set[Atom[Int]](), Set(a)), Clause(Set(c), Set(b)))
  val testProblem = MaxSatProblem[Int](hardClauses = testClauses, softClauses = Set())
}
