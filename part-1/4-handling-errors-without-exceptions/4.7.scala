object EitherSequenceAndTraverseSolution extends App {
  def traverse[A, B, E](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    a.foldLeft(Right(List()): Either[E, List[B]]) { (acc, a) =>
      acc match {
        case Left(e) => acc
        case Right(gotAcc) => f(a) match {
          case Left(e) => Left(e)
          case Right(b) => Right(gotAcc :+ b)
        }
      }
    }
  }

  def doubleEnsuringOdd(x: Int) = if (x % 2 == 1) Right(x * 2) else Left("Not odd")
  assert(traverse(List(1, 3, 5))(doubleEnsuringOdd) == Right(List(2, 6, 10)))
  assert(traverse(List(1, 2, 5))(doubleEnsuringOdd) == Left("Not odd"))


  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(a)(identity)
  }

  assert(sequence(List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
  assert(sequence(List(Right(1), Left("Error"), Right(3))) == Left("Error"))
  assert(sequence(List(Right(1), Left("Error 1"), Left("Error 2"))) == Left("Error 1"))
}
