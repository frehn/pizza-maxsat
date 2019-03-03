package util

object rectangle {
  def apply[T1, T2](seq1: Seq[T1], seq2: Seq[T2]): Seq[(T1, T2)] =
    seq1.flatMap(o1 =>
      seq2.foldLeft(Seq[(T1, T2)]()) { case (result, o2) => result :+ (o1, o2) }
    )
}
