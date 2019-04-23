package maxsat.solver

import maxsat.{Atom, MaxSatSolution}

object dimacsSolutionToMaxSatSolution {
  def apply[T](solution: Seq[Int], variableMap: Map[Atom[T], Int]): MaxSatSolution[T] = {
    val myModel: Map[Atom[T], Boolean] = variableMap.map { case (atom, i) =>
      if (solution.contains(i))
        atom -> true
      else
        atom -> false
    }
    MaxSatSolution(myModel, variableMap)
  }
}
