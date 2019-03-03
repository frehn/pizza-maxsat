package pizza

import maxsat.MaxSatSolution

object maxSatSolutionToPizzaSolution {
  def apply(solution: MaxSatSolution[PizzaAtom]): PizzaSolution = {
    /*solution.model.foreach{ case (atom, tru) => {
      val str = if (tru) "" else "Not "
      atom.id match {
        case SliceChosen(slice) => println(s"${str}slice chosen: ${slice}")
        case MushroomAt(i, j) => println(s"${str}mushroom at: $i, $j")
        case TomatoAt(i, j) => println(s"${str}tomato at: $i, $j")
        case CellBelongs(i, j) => println(s"${str}cell belongs: $i, $j")
      }
    }}*/

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
