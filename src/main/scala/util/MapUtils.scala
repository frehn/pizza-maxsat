package util

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import org.slf4j.Logger

import scala.collection.parallel.ParSeq

object MapUtils {

  implicit class LoggingParallelMap[T](seq: ParSeq[T]) {
    def mapAndLogProgress[S](f: T => S)(implicit logger: Logger): ParSeq[S] = {
      val finishedCnt = new AtomicInteger(0)
      setupLogging(finishedCnt)

      seq.map(x => {
        val res = f(x)
        finishedCnt.incrementAndGet()
        res
      })
    }

    def flatMapAndLogProgress[S](f: T => Seq[S])(implicit logger: Logger): ParSeq[S] = {
      val finishedCnt = new AtomicInteger(0)
      setupLogging(finishedCnt)

      seq.flatMap(x => {
        val res = f(x)
        finishedCnt.incrementAndGet()
        res
      })
    }

    private def setupLogging(finishedCnt: AtomicInteger)(implicit logger: Logger) = {
      val pool = Executors.newScheduledThreadPool(1)
      pool.scheduleWithFixedDelay(() => {
        val cnt = finishedCnt.get
        if (cnt == seq.length)
          pool.shutdown()
        else
          logger.info(f"${cnt / seq.length.toFloat}%2.2f done")
      }, 10, 10, TimeUnit.SECONDS)
    }
  }

}
