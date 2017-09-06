object Solution515 extends App {
  def tails[A](input: Stream[A]) = Solution511.unfold(input) { (state) =>
    state match {
      case null => None
      case Empty => Some(Stream.empty, null)
      case Cons(_, tail) => Some(state, tail())
    }
  }

  val input = Stream.cons(1, Stream.cons(2, Stream.empty))

  val actual = tails(input)
  assert(Solution51.toList(actual).length == 3)
  assert(Stream.isEqual(Solution51.toList(actual)(0), Stream.cons(1, Stream.cons(2, Stream.empty))))
  assert(Stream.isEqual(Solution51.toList(actual)(1), Stream.cons(2, Stream.empty)))
  assert(Stream.isEqual(Solution51.toList(actual)(2), Stream.empty))
}
