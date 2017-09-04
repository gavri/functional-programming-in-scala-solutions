object Solution51 extends App {
  def toList[A](stream: Stream[A]): List[A] = stream match {
    case Cons(head, tail) => head() :: toList(tail())
    case _ => Nil
  }

  assert(toList(Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))) == List(1, 2, 3))
  assert(toList(Stream.empty) == List())
}
