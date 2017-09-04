object Solution55 extends App {
  def foldRight[A, B](stream: Stream[A])(z: => B)(f: (A, => B) => B): B = stream match {
    case Cons(h,t) => f(h(), foldRight(t())(z)(f))
    case _ => z
  }

  def takeWhile[A](stream: Stream[A], p: (A) => Boolean): Stream[A] = {
    foldRight(stream)(Stream.empty: Stream[A])((e, acc) => if (p(e)) Stream.cons(e, acc) else Stream.empty)
  }

  assert(Stream.isEqual(takeWhile(Stream.cons(1, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))), ((x: Int) => x % 2 == 1)), Stream.cons(1, Stream.cons(3, Stream.empty))))

  assert(Stream.isEqual(takeWhile(Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))), ((x: Int) => x % 2 == 1)), Stream.empty))

  assert(Stream.isEqual(takeWhile(Stream.empty, ((x: Int) => x % 2 == 1)), Stream.empty))
}
