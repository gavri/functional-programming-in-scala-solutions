object Solution513 extends App {
  def map[A, B](input: Stream[A], f: (A) => B): Stream[B] = Solution511.unfold(input)((state) => state match {
    case Empty => None
    case Cons(head, tail) => Some((f(head()), tail()))
  })
  assert(Stream.isEqual(map(Stream.cons(1, Stream.cons(2, Stream.empty)), ((x: Int) => x * 2)), Stream.cons(2, Stream.cons(4, Stream.empty))))

  def take[A](stream: Stream[A], n: Int): Stream[A] = Solution511.unfold((n, stream)) { state =>
    state match {
      case (0, _) => None
      case (_, Empty) => None
      case (n, Cons(head, tail)) => Some((head(), (n - 1, tail())))
    }
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

  def takeWhile[A](stream: Stream[A], p: (A) => Boolean): Stream[A] = Solution511.unfold(stream)(state => state match {
    case Empty => None
    case Cons(head, tail) => if (p(head())) Some((head(), tail())) else None
  })

  assert(Stream.isEqual(takeWhile(Stream.cons(1, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))), ((x: Int) => x % 2 == 1)), Stream.cons(1, Stream.cons(3, Stream.empty))))
  assert(Stream.isEqual(takeWhile(Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))), ((x: Int) => x % 2 == 1)), Stream.empty))
  assert(Stream.isEqual(takeWhile(Stream.empty, ((x: Int) => x % 2 == 1)), Stream.empty))

  def zipWith[A, B, C](left: Stream[A], right: Stream[B])(f: (A, B) => C): Stream[C] = Solution511.unfold((left, right)) {(state) =>
    state match {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(leftHead, leftTail), Cons(rightHead, rightTail)) => Some(f(leftHead(), rightHead()), (leftTail(), rightTail()))
    }
  }

  assert(Stream.isEqual(
    zipWith(Stream.cons(1, Stream.cons(2, Stream.empty)), Stream.cons(10, Stream.cons(11, Stream.empty)))(_ + _),
    Stream.cons(11, Stream.cons(13, Stream.empty))
  ))

  assert(Stream.isEqual(
    zipWith(Stream.cons(1, Stream.empty), Stream.cons(10, Stream.cons(11, Stream.empty)))(_ + _),
    Stream.cons(11, Stream.empty)
  ))

  assert(Stream.isEqual(
    zipWith(Stream.cons(1, Stream.cons(2, Stream.empty)), Stream.cons(11, Stream.empty))(_ + _),
    Stream.cons(12, Stream.empty)
  ))

  def zipAll[A, B](left: Stream[A], right: Stream[B]): Stream[(Option[A], Option[B])] = Solution511.unfold((left, right)) {
    case ((Empty, Empty)) => None
    case ((Cons(leftHead, leftTail), Empty)) => Some(((Some(leftHead()), None), (leftTail(), Empty)))
    case ((Empty, Cons(rightHead, rightTail))) => Some(((None, Some(rightHead())), (Empty, rightTail())))
    case (Cons(leftHead, leftTail), Cons(rightHead, rightTail)) => Some((Some(leftHead()), Some(rightHead())), (leftTail(), rightTail()))
  }

  assert(Stream.isEqual(
    zipAll(Stream.cons(1, Stream.cons(2, Stream.empty)), Stream.cons(3, Stream.cons(4, Stream.empty))),
    Stream.cons((Some(1), Some(3)), Stream.cons((Some(2), Some(4)), Stream.empty))
    ))

  assert(Stream.isEqual(
    zipAll(Stream.cons(1, Stream.empty), Stream.cons(3, Stream.cons(4, Stream.empty))),
    Stream.cons((Some(1), Some(3)), Stream.cons((None, Some(4)), Stream.empty))
    ))

  assert(Stream.isEqual(
    zipAll(Stream.cons(1, Stream.cons(2, Stream.empty)), Stream.cons(3, Stream.empty)),
    Stream.cons((Some(1), Some(3)), Stream.cons((Some(2), None), Stream.empty))
    ))
}
