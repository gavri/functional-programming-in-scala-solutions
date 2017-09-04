object Solution53 extends App {

  def takeWhile[A](stream: Stream[A], p: (A) => Boolean): Stream[A] = stream match {
    case Empty => Stream.empty
    case Cons(head, tail) => if (p(head())) Cons(head, () => takeWhile(tail(), p)) else Stream.empty
  }

  assert(Stream.isEqual(takeWhile(Stream.cons(1, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))), ((x: Int) => x % 2 == 1)), Stream.cons(1, Stream.cons(3, Stream.empty))))

  assert(Stream.isEqual(takeWhile(Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))), ((x: Int) => x % 2 == 1)), Stream.empty))

  assert(Stream.isEqual(takeWhile(Stream.empty, ((x: Int) => x % 2 == 1)), Stream.empty))
}
