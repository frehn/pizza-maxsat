package maxsat

sealed trait Formula[+T]
sealed trait Literal[+T]

case class Atom[+T](id: T) extends Formula[T]
case class And[+T](conjuncts: Seq[Formula[T]]) extends Formula[T]
case class Eq[+T](left: Formula[T], right: Formula[T]) extends Formula[T]
case class Imp[+T](left: Formula[T], right: Formula[T]) extends Formula[T]
case class Not[+T](f: Formula[T]) extends Formula[T]
case class Or[+T](disjuncts: Seq[Formula[T]]) extends Formula[T]