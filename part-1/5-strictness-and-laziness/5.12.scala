object Solution512 extends App {
  def fibs: Stream[Int] = Solution511.unfold((0, 1))((state) => Some(state._1, (state._2, state._1 + state._2)))
  assert(Solution51.toList(Solution52.take(fibs, 10)) == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))

  def from(n: Int): Stream[Int] = Solution511.unfold(n)((state) => Some(state, state + 1))
  assert(Solution51.toList(Solution52.take(from(20), 7)) == List(20, 21, 22, 23, 24, 25, 26))

  def constant(n: Int): Stream[Int] = Solution511.unfold(n)((state) => Some(state, state))
  assert(Solution51.toList(Solution52.take(constant(4), 7)) == List(4, 4, 4, 4, 4, 4, 4))

  def ones: Stream[Int] = Solution511.unfold(null)(_ => Some(1, null))
  assert(Solution51.toList(Solution52.take(ones, 7)) == List(1, 1, 1, 1, 1, 1, 1))
}
