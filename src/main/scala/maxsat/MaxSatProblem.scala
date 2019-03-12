package maxsat

import akka.NotUsed
import akka.stream.scaladsl.Source

class MaxSatProblem[T](val hardClauses: Source[Clause[T], NotUsed], val softClauses: Set[Clause[T]]) {
  def clauses : Source[Clause[T], NotUsed] = hardClauses.concat(Source(softClauses))
}

object MaxSatProblem {
  def apply[T](hardClauses: Source[Clause[T], NotUsed], softClauses: Set[Clause[T]]) = new MaxSatProblem(hardClauses, softClauses)
}