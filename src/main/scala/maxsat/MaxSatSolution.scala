package maxsat

case class MaxSatSolution[T](model: Map[Atom[T], Boolean])