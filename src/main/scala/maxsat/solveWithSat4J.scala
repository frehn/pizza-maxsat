package maxsat

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}

import org.sat4j.maxsat.{SolverFactory, WeightedMaxSatDecorator}
import org.sat4j.maxsat.reader.WDimacsReader

object solveWithSat4J {
  def apply[T](problem: MaxSatProblem[T]): Option[MaxSatSolution[T]] = {
    val solver = new WeightedMaxSatDecorator(SolverFactory.newDefault())
    solver.setTimeout(3600)
    val reader = new WDimacsReader(solver)

    val file = File.createTempFile("pizza-maxsat", ".dimacs")

    val variableMap = maxSatProblemToDimacs(problem, new FileOutputStream(file))

    System.out.println("Problem")
    System.out.println(scala.io.Source.fromInputStream(new FileInputStream(file)).mkString)

    val solverProblem = reader.parseInstance(new FileInputStream(file))
    Option(solverProblem.findModel()) match {
      case None => {
        System.out.println("No model found")
        None
      }
      case Some(model) => {
        val myModel: Map[Atom[T], Boolean] = variableMap.map { case (atom, i) => {
          if (model.contains(i))
            atom -> true
          else
            atom -> false
        }
        }
        Some(MaxSatSolution(myModel))
      }
    }
  }
}