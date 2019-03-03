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

    if (x1 <= x2) { // slice 1 left slice 2
      checkAssumingLeft(x1, x2, y1, y2, length1, width1, width2)
    } else { // slice 2 left slice 1
      checkAssumingLeft(x2, x1, y2, y1, length2, width2, width1)
    }
  }

  private def checkAssumingLeft(x1: Int, x2: Int, y1: Int, y2: Int, length1: Int, width1: Int, width2: Int) = {
    if (x1 + length1 <= x2) {
      false
    } else if (y1 <= y2) { // slice 1 above left slice2
      y2 <= y1 + width1
    } else { // slice 1 below left slice2
      y1 <= y2 + width2
    }
  }
}
