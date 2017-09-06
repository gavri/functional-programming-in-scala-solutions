object Solution59 extends App {
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  assert(Stream.isEqual(
    Solution52.take(from(5), 3),
    Stream.cons(5, Stream.cons(6, Stream.cons(7, Stream.empty)))
  ))
}
