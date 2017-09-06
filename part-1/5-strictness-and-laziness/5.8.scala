object Solution58 extends App {
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  assert(Stream.isEqual(
    Solution52.take(constant(5), 3),
    Stream.cons(5, Stream.cons(5, Stream.cons(5, Stream.empty)))
  ))
}
