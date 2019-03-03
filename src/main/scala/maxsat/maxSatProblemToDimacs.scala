package maxsat

import java.io.{OutputStream, OutputStreamWriter, PrintWriter}

object maxSatProblemToDimacs {
  def apply[T](maxSatProblem: MaxSatProblem[T],
               out: OutputStream): Map[Atom[T], Int] = {
    val writer = new PrintWriter(new OutputStreamWriter(out))
    val clauseNum = maxSatProblem.clauses.size
    val top = maxSatProblem.softClauses.size + 1

    val variables = getVariables(maxSatProblem)
    val variableMap = variables.toSeq.zipWithIndex.map { case (v, i) => v -> (i + 1) }.toMap

    writer.println(s"p wcnf ${variables.size} $clauseNum $top")

    val lines = maxSatProblem.hardClauses.map(c => dimacsLine(top, c, variableMap)).toSeq ++
      maxSatProblem.softClauses.map(c => dimacsLine(1, c, variableMap)).toSeq

    writer.print(lines.mkString("\n"))

    writer.close()

    variableMap
  }

  private def dimacsLine[T](weight: Int, c: Clause[T], variableMap: Map[Atom[T], Int]): String = {
    s"$weight ${c.positive.map(atom => variableMap(atom)).mkString(" ")} ${c.negative.map(atom => s"-${variableMap(atom)}").mkString(" ")} 0"
  }

  private[maxsat] def getVariables[T](maxSatProblem: MaxSatProblem[T]): Set[Atom[T]] =
    maxSatProblem.clauses.flatMap(clause => clause.positive ++ clause.negative)
}