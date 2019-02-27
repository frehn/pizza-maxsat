package maxsat

class MaxSatProblem[T](val hardClauses: Set[Clause[T]], val softClauses: Set[Clause[T]]) {
  def clauses : Set[Clause[T]] = hardClauses ++ softClauses
}

object MaxSatProblem {
  def apply[T](hardClauses: Set[Clause[T]], softClauses: Set[Clause[T]]) = new MaxSatProblem(hardClauses, softClauses)
}