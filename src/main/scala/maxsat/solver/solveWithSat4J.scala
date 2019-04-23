package maxsat.solver

import java.io.{File, FileInputStream}

import maxsat.{MaxSatProblem, MaxSatSolution, maxSatProblemToDimacs}
import org.sat4j.maxsat.reader.WDimacsReader
import org.sat4j.maxsat.{SolverFactory, WeightedMaxSatDecorator}
import org.slf4j.{Logger, LoggerFactory}

object solveWithSat4J {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  def apply[T](problem: MaxSatProblem[T]): Option[MaxSatSolution[T]] = {
    val solver = new WeightedMaxSatDecorator(SolverFactory.newDefault())
    solver.setTimeout(3600)
    val reader = new WDimacsReader(solver)

    val file = File.createTempFile("pizza-maxsat", ".dimacs")

    val variableMap = maxSatProblemToDimacs(problem, file)

    logger.info("Solving with SAT4J")

    val solverProblem = reader.parseInstance(new FileInputStream(file))
    Option(solverProblem.findModel()) match {
      case None => None
      case Some(model) => Some(dimacsSolutionToMaxSatSolution(model, variableMap))
    }
  }
}
