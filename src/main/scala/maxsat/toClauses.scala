package maxsat

import akka.NotUsed
import akka.stream.scaladsl.Source
import gapt.expr.fol.TseitinCNF
import gapt.expr.{FOLAtom, FOLFormula}

object toClauses {
  def apply[T](f: Formula[T]): Source[Clause[T], NotUsed] =
    Source(rec(f))

  def rec[T](f: Formula[T]): Set[Clause[T]] = {
    f match {
      case a@Atom(_) => Set(Clause(negative = Set(), positive = Set(a)))
      case And(conjuncts) => conjuncts.flatMap(f => rec(f)).toSet
      case Eq(left, right) => rec(And(Seq(Imp(left, right), Imp(right, left))))
      case Imp(left, right) => rec(Or(Seq(Not(left), right)))
      case Not(a@Atom(_)) => Set(Clause(negative = Set(a), positive = Set()))
      case Not(Not(g)) => rec(g)
      case Not(Or(disjuncts)) => rec(And(disjuncts.map(Not(_))))
      case Not(And(conjuncts)) => rec(Or(conjuncts.map(Not(_))))
      case Not(Imp(left, right)) => rec(And(Seq(left, Not(right))))
      case Not(Eq(left, right)) => rec(Not(And(Seq(Imp(left, right), Imp(right, left)))))
      case Or(disjuncts) => merge(disjuncts.map(d => rec(d)))
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
    clauseSet1.flatMap(clause1 => clauseSet2.map(clause2 => merge(clause1, clause2)))
  }

  private def merge[T](clause1: Clause[T], clause2: Clause[T]): Clause[T] =
    Clause(positive = clause1.positive ++ clause2.positive, negative = clause1.negative ++ clause2.negative)

  private def fromGaptClause[T](clause: gapt.proofs.FOLClause) : Clause[T] =
    // TODO: how to translate back?

  private def toClausesUsingGapt[T](f: Formula[T]): Set[Clause[T]] =
    TseitinCNF(toGaptFormula(f)).map(fromGaptClause[T]).toSet

  private def toGaptFormula[T](f: Formula[T]): FOLFormula = f match {
    case a@Atom(_) => FOLAtom(a.toString)
    case And(conjuncts) => gapt.expr.And(conjuncts.map(f => toGaptFormula(f)))
    case Eq(left, right) => gapt.expr.And(Seq(gapt.expr.Imp(toGaptFormula(left), toGaptFormula(right)), gapt.expr.Imp(toGaptFormula(right), toGaptFormula(left)))))
    case Imp(left, right) => gapt.expr.Imp(toGaptFormula(left), toGaptFormula(right))
    case Not(f) => gapt.expr.Neg(toGaptFormula(f))
    case Or(disjuncts) => gapt.expr.Or(disjuncts.map(f => toGaptFormula(f)))
  }
}