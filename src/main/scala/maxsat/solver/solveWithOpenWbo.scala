package maxsat.solver

import java.io.File

import maxsat.solver.solveWithSat4J.logger
import maxsat.{Atom, MaxSatProblem, MaxSatSolution, maxSatProblemToDimacs}

import scala.sys.process._

object solveWithOpenWbo {
  def apply[T](problem: MaxSatProblem[T]): Option[MaxSatSolution[T]] = {
    val file = File.createTempFile("pizza-maxsat", ".dimacs")

    implicit val variableMap: Map[Atom[T], Int] = maxSatProblemToDimacs(problem, file)

    logger.info("Solving with OpenWBO")

    val output = s"open-wbo ${file.getAbsolutePath}".lineStream_!

    parseOutput(output)
  }

  private def parseOutput[T](lines: Seq[String])(implicit variableMap: Map[Atom[T], Int]): Option[MaxSatSolution[T]] = {
    val i = lines.indexWhere(_.startsWith("v"))

    if (i == -1)
      None
    else
      Some(parseSolution(lines(i)))
  }

  private def parseSolution[T](string: String)(implicit variableMap: Map[Atom[T], Int]): MaxSatSolution[T] =
    dimacsSolutionToMaxSatSolution(string.split(" ").drop(1).map(_.toInt), variableMap)
}