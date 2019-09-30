package maxsat

import akka.NotUsed
import akka.stream.scaladsl.Source

object toClauses {
  def apply[T](f: Formula[T]): Source[Clause[T], NotUsed] = {
    f match {
      case a @ Atom(_) => Source.fromIterator(() => Set(Clause(Set[Atom[T]](), Set(a))).iterator)
      case And(conjuncts) => Source.fromIterator(() => conjuncts.iterator).flatMapMerge(2, f => apply(f) )
      case Eq(left, right) => apply(And(Seq(Imp(left, right), Imp(right, left))))
      case Imp(left, right) => apply(Or(Seq(Not(left), right)))
      case Not( a @ Atom(_)) => Source.fromIterator(() => Set(Clause(Set(a), Set[Atom[T]]())).iterator)
      case Not(Not(g)) => apply(g)
      case Not(Or(disjuncts)) => apply(And(disjuncts.map(Not(_))))
      case Not(And(conjuncts)) => apply(Or(conjuncts.map(Not(_))))
      case Not(Imp(left, right)) => apply(And(Seq(left, Not(right))))
      case Not(Eq(left, right)) => apply(Not(And(Seq(Imp(left, right), Imp(right, left)))))
      case Or(disjuncts) => merge(disjuncts.map(d => apply(d)))
    }
  }

  private def merge[T](clauseSets: Seq[Source[Clause[T], NotUsed]]): Source[Clause[T], NotUsed] = {
    if (clauseSets.length == 1)
      clauseSets.head
    else {
      merge(clauseSets.head, merge(clauseSets.tail))
    }
  }

  private def merge[T](clauseSet1: Source[Clause[T], NotUsed], clauseSet2: Source[Clause[T], NotUsed]): Source[Clause[T], NotUsed] = {
    clauseSet1.flatMapMerge( 4, clause1 => clauseSet2.map( clause2 => merge(clause1, clause2)))
  }

  private def merge[T](clause1: Clause[T], clause2: Clause[T]): Clause[T] =
    Clause(positive = clause1.positive ++ clause2.positive, negative = clause1.negative ++ clause2.negative)
}