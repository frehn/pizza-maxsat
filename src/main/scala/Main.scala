import java.io.FileInputStream

import org.slf4j.Logger
import maxsat.{Atom, MaxSatSolution, solveWithSat4J}
import org.slf4j.LoggerFactory
import pizza._
import pizza.parser.parsePizzaProblem

object Main extends App {


  override def main(args: Array[String]): Unit = {
    val logger = LoggerFactory.getLogger(getClass)
    if (args.length != 1) {
      println("USAGE: pizza-maxsat [FILE]")
      System.exit(1)
    }

    val problem = parsePizzaProblem(scala.io.Source.fromInputStream(new FileInputStream(args(0))).mkString)
    logger.info("Precomputing cells and valid slices...")
    val data = pizzaProblemToData(problem)

    logger.info("Translating to MaxSAT...")
    val maxSatProblem = pizzaProblemToMaxSatProblem(data)

    solveWithSat4J(maxSatProblem) match {
      case Some(maxSatSolution) =>
        val solution = maxSatSolutionToPizzaSolution(maxSatSolution)
        logger.info("Solution")
        logger.info("--------")
        printSolutionStatusInfo(data, maxSatSolution, solution, logger)
        logger.info("")
        logger.info(s"Number of slices: ${solution.slices.size}")
        val score = scoreSolution(solution)
        logger.info(s"Number of cells on slices: $score")
        logger.info(s"Number of cells not on slices: ${problem.columns * problem.rows - score}")
        logger.info("")
        logger.info("Details")
        logger.info("-------")
        solution.slices.foreach(slice => logger.info(slice.toString))
      case None => logger.info("No solution found")
    }
  }

  private def printSolutionStatusInfo(data: PizzaProblemData, maxSatSolution: MaxSatSolution[PizzaAtom], pizzaSolution: PizzaSolution, logger: Logger): Unit = {
    val status = getSolutionStatus(data, pizzaSolution)
    logger.info(s"Solution status: $status")

    status match {
      case OverlappingSlices(slices) =>
        slices.foreach { case (s1, s2) =>
          logSliceModelInfo(maxSatSolution, s1, logger)
          logSliceModelInfo(maxSatSolution, s2, logger)
        }
      case _ =>
    }


  }

  private def logSliceModelInfo(maxSatSolution: MaxSatSolution[PizzaAtom], slice: Slice, logger: Logger): Unit = {
    logger.info(s"$slice chosen ~ ${maxSatSolution.variableMap(Atom(SliceChosen(slice)))}")
  }
}