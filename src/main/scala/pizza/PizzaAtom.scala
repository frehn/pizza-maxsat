package pizza

sealed trait PizzaAtom

case class CellBelongs(i: Int, j: Int) extends PizzaAtom
case class MushroomAt(i: Int, j: Int) extends PizzaAtom
case class TomatoAt(i: Int, j: Int) extends PizzaAtom
case class SliceChosen(slice: Slice) extends PizzaAtom