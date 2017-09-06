object Solution511 extends App {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty
    case Some((e, newZ)) => Stream.cons(e, unfold(newZ)(f))
  }

  assert(Stream.isEqual(
    Solution52.take(unfold(0)((x: Int) => Some(x + 1, x + 1)), 4),
    Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.empty))))
  ))
}
