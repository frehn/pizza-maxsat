package util

object rectangle {
  def apply[T1, T2](seq1: Seq[T1], seq2: Seq[T2]) : Seq[(T1, T2)] =
    seq1.foldLeft(Seq[(T1, T2)]()){ case (rectangle, o1) =>
    val innerRect = seq2.foldLeft(Seq[(T1, T2)]()){ case (innerRect, o2)  => {
      innerRect :+ (o1, o2)
    }}
    rectangle ++ innerRect
  }
}
