package maxsat

case class Clause[T](negative: Set[Atom[T]], positive: Set[Atom[T]])
