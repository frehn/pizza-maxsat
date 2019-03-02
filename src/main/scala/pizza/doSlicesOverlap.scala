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
      if (x1 + length1 <= x2) {
        return false
      }

      if (y1 <= y2) { // slice 1 above left slice2
        return y2 <= y1 + width1
      } else { // slice 1 below left slice2
        return y1 <= y2 + width2
      }
    } else { // slice 2 left slice 1
      if (x2 + length2 <= x1) {
        return false
      }

      if (y2 <= y1) { // slice 2 above left slice 1
        return y1 <= y2 + width2
      } else { // slice 1 below left slice2
        return y2 <= y1 + width1
      }
    }
  }
}
