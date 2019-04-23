package maxsat

package object solver {
  type SolverFunction[T] = MaxSatProblem[T] => Option[MaxSatSolution[T]]
}
