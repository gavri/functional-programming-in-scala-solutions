object Solution52 extends App {
  def take[A](stream: Stream[A], n: Int): Stream[A] = (stream, n) match {
    case (_, 0) => Stream.empty
    case (Empty, _) => Stream.empty
    case (Cons(head, tail), n) => Cons(head, () => take(tail(), n - 1))
  }

  def drop[A](stream: Stream[A], n: Int): Stream[A] = (stream, n) match {
    case (_, 0) => stream
    case (Empty, _) => stream
    case (Cons(_, tail), n) => drop(tail(), n - 1)
  }

  assert(Stream.isEqual(
    take(Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty))), 2),
    Stream.cons(1, Stream.cons(2, Stream.empty))
  ))

  assert(Stream.isEqual(
    take(Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty))), 10),
    Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
  ))

  assert(Stream.isEqual(
    take(Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty))), 0),
    Stream.empty
  ))

  assert(Stream.isEqual(
    drop(Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty))), 2),
    Stream.cons(3, Stream.empty)
  ))

  assert(Stream.isEqual(
    drop(Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty))), 10),
    Stream.empty
  ))

  assert(Stream.isEqual(
    drop(Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty))), 0),
    Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
  ))
}
