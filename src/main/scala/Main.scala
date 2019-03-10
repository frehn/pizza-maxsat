import java.io.FileInputStream

import maxsat.solveWithSat4J
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

    logger.info("Solving with SAT4J...")
    solveWithSat4J(maxSatProblem) match {
      case Some(maxSatSolution) =>
        val solution = maxSatSolutionToPizzaSolution(maxSatSolution)
        logger.info("Solution")
        logger.info("--------")
        logger.info(s"Solution status: ${getSolutionStatus(data, solution)}")
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
}