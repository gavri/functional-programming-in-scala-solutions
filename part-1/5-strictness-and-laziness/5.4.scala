object Solution54 extends App {
  def forAll[A](stream: Stream[A])(p: A => Boolean): Boolean = stream match {
    case Empty => true
    case Cons(head, tail) => p(head()) && forAll(tail())(p)
  }

  assert(forAll(Stream.cons(1, Stream.cons(3, Stream.empty)))(_ % 2 == 1))
  assert(!forAll(Stream.cons(1, Stream.cons(2, Stream.empty)))(_ % 2 == 1))
}
