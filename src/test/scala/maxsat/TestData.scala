package maxsat

object TestData {
  private val a = Atom[Int](1)
  private val b = Atom[Int](2)
  private val c = Atom[Int](3)

  val hardClauses: Set[Clause[Int]] = Set(Clause(Set(a), Set(b)), Clause(Set[Atom[Int]](), Set(a)), Clause(Set(c), Set(b)))
  val problem: MaxSatProblem[Int] = MaxSatProblem[Int](hardClauses = hardClauses, softClauses = Set())
}
