package pizza

object cellContainedIn {
  def apply(cell: Cell, slice: Slice): Boolean = {
    slice.upperLeft.x <= cell.x && cell.x <= slice.upperLeft.x + slice.length - 1 &&
      slice.upperLeft.y <= cell.y && cell.y <= slice.upperLeft.y + slice.width - 1
  }
}
