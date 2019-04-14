package maxsat

import java.io._
import java.nio.charset.Charset
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration


object maxSatProblemToDimacs {
  private val logger = LoggerFactory.getLogger(getClass)

  def apply[T](maxSatProblem: MaxSatProblem[T],
               outFile: File): Map[Atom[T], Int] = {
    implicit val system: ActorSystem = ActorSystem("Output")
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val top = maxSatProblem.softClauses.size + 1
    val clauseNum = new AtomicInteger(maxSatProblem.softClauses.size)

    val variableMap = mutable.Map[Atom[T], Int]()

    val tempFile = File.createTempFile("pizza-maxsat", ".pre.dimacs")

    logger.info(s"Writing DIMACS to ${tempFile.getAbsolutePath}")

    val future = maxSatProblem.hardClauses.map(c => {
      clauseNum.incrementAndGet()
      dimacsLine(top, c, variableMap)
    }).concat(
      Source(maxSatProblem.softClauses.map(c => dimacsLine(1, c, variableMap))))
      .map(t => ByteString(t + "\n"))
      .runWith(FileIO.toPath(tempFile.toPath))

    Await.ready(future, Duration.Inf)
    system.terminate()

    logger.debug("Preparing final DIMACS file.")
    writeDimacsFile(tempFile, top, clauseNum, variableMap, outFile)

    logger.info("DIMACS written.")

    variableMap.toMap
  }

  private def writeDimacsFile[T](tempFile: File, top: Int, clauseNum: AtomicInteger, variableMap: mutable.Map[Atom[T], Int], outFile: File) = {
    val firstLine = IOUtils.toInputStream(s"p wcnf ${variableMap.size} ${clauseNum.get} $top\n", Charset.forName("UTF8"))
    val rest = new FileInputStream(tempFile)

    val inStream = new SequenceInputStream(firstLine, rest)
    val outStream = new FileOutputStream(outFile)

    IOUtils.copy(inStream, outStream)
  }

  private def dimacsLine[T](weight: Int, c: Clause[T], variableMap: mutable.Map[Atom[T], Int]): String = {
    s"$weight ${c.positive.map(atom => variableMap.getOrElseUpdate(atom, variableMap.size + 1)).mkString(" ")} ${c.negative.map(atom => s"-${variableMap.getOrElseUpdate(atom, variableMap.size + 1)}").mkString(" ")} 0"
  }
}