package pizza

import maxsat.MaxSatSolution

object maxSatSolutionToPizzaSolution {
  def apply(solution: MaxSatSolution[PizzaAtom]): PizzaSolution = {
    PizzaSolution(
      solution.model
        .filter { case (atom, tru) => tru && (atom.id match {
          case SliceChosen(_) => true
          case _ => false
        })
        }
        .map { case (atom, _) => atom.id.asInstanceOf[SliceChosen].slice }
        .toSeq
    )
  }
}
