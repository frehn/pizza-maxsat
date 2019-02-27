package maxsat

object toClauses {
  def apply[T](f: Formula[T]): Set[Clause[T]] = {
    f match {
      case a @ Atom(_) => Set(Clause(negative = Set(), positive = Set(a)))
      case And(conjuncts) => conjuncts.flatMap( f => apply(f) ).toSet
      case Eq(left, right) => apply(And(Seq(Imp(left, right), Imp(right, left))))
      case Imp(left, right) => apply(Or(Seq(Not(left), right)))
      case Not( a @ Atom(_)) => Set(Clause(negative = Set(a), positive = Set()))
      case Not(Not(f)) => apply(f)
      case Not(Or(disjuncts)) => apply(And(disjuncts.map(Not(_))))
      case Not(And(conjuncts)) => apply(Or(conjuncts.map(Not(_))))
      case Not(Imp(left, right)) => apply(And(Seq(left, Not(right))))
      case Not(Eq(left, right)) => apply(Not(And(Seq(Imp(left, right), Imp(right, left)))))
      case Or(disjuncts) => merge(disjuncts.map(d => apply(d)))
    }
  }

  private def merge[T](clauseSets: Seq[Set[Clause[T]]]): Set[Clause[T]] = {
    if (clauseSets.length == 1)
      clauseSets.head
    else {
      merge(clauseSets.head, merge(clauseSets.tail))
    }
  }

  private def merge[T](clauseSet1: Set[Clause[T]], clauseSet2: Set[Clause[T]]): Set[Clause[T]] = {
    clauseSet1.flatMap( clause1 => clauseSet2.map( clause2 => merge(clause1, clause2)))
  }

  private def merge[T](clause1: Clause[T], clause2: Clause[T]): Clause[T] =
    Clause(positive = clause1.positive ++ clause2.positive, negative = clause1.negative ++ clause2.negative)
}