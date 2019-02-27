package pizza

object doSlicesOverlap {
  def apply(slice1: Slice, slice2: Slice): Boolean = {
    val x1 = slice1.upperLeft.x
    val x2 = slice2.upperLeft.x
    val y1 = slice1.upperLeft.y
    val y2 = slice2.upperLeft.y
    val length1 = slice1.length
    val length2 = slice2.length
    val width1 = slice1.width
    val width2 = slice2.width

    if (x1 <= x2) {
      x1 + length1 > x2 && y2 <= y1 && y1 <= y2 + width2
    } else { // x2 < x1
      x2 + length2 > x1 && y1 <= y2 && y2 <= y1 + width1
    }
  }
}
