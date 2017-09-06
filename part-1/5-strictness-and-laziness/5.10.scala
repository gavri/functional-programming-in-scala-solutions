object Solution510 extends App {
  def fibs: Stream[Int] = {
    lazy val Cons(_, tail) = fibs
    Stream.cons(0, Stream.cons(1, zipWith(fibs, tail())(_ + _)))
  }

  private def zipWith[A, B, C](left: Stream[A], right: Stream[B])(f: (A, B) => C): Stream[C] = (left, right) match {
    case (Empty, _) => Stream.empty
    case (_, Empty) => Stream.empty
    case (Cons(leftHead, leftTail), Cons(rightHead, rightTail)) => Stream.cons(f(leftHead(), rightHead()), zipWith(leftTail(), rightTail())(f))
  }

  assert(Solution51.toList(Solution52.take(fibs, 7)) == List(0, 1, 1, 2, 3, 5, 8))
}
