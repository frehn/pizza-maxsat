package object pizza {
  val testProblem = PizzaProblem(R = 3,
    C = 5,
    L = 1,
    H = 6,
    (x, y) => {
      if (x == 0 || x == 2)
        Tomato()
      else if (1 <= y && y <= 3)
        Mushroom()
      else
        Tomato()
    })
}
