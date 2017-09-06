object Solution57 extends App {
  def foldRight[A, B](stream: Stream[A])(z: => B)(f: (A, => B) => B): B = stream match {
    case Cons(h,t) => f(h(), foldRight(t())(z)(f))
    case _ => z
  }

  def map[A, B](stream: Stream[A], f: (A) => B): Stream[B] = {
    foldRight(stream)(Stream.empty: Stream[B])((e, acc) => Stream.cons(f(e), acc))
  }

  assert(Stream.isEqual(map(Stream.cons(1, Stream.cons(2, Stream.empty)), (x: Int) => x * 2), Stream.cons(2, Stream.cons(4, Stream.empty))))

  def filter[A](stream: Stream[A], f: (A) => Boolean): Stream[A] = {
    foldRight(stream)(Stream.empty: Stream[A])((e, acc) => if (f(e)) Stream.cons(e, acc) else acc)
  }

  assert(Stream.isEqual(filter(Stream.cons(1, Stream.cons(2, Stream.cons(4, Stream.empty))), (x: Int) => x % 2 == 0), Stream.cons(2, Stream.cons(4, Stream.empty))))

  def append[A](left: Stream[A], right: => Stream[A]) = {
    foldRight(left)(right)((e, acc) => Stream.cons(e, acc))
  }

  assert(Stream.isEqual(append(Stream.cons(1, Stream.cons(2, Stream.empty)), Stream.cons(3, Stream.cons(4, Stream.empty))), Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.empty))))))

  def flatMap[A](stream: Stream[Stream[A]]) = {
    foldRight(stream)(Stream.empty: Stream[A])((e, acc) => append(e, acc))
  }

  assert(Stream.isEqual(
    flatMap(Stream.cons(Stream.cons(1, Stream.cons(2, Stream.empty)), Stream.cons(Stream.cons(3, Stream.cons(4, Stream.empty)), Stream.empty))),
    Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.empty))))))

}
