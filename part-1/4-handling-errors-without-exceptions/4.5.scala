object TraverseSolution extends App {
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(Some(List()): Option[List[B]]) { (acc, e) =>
      acc.flatMap { (gotAcc) =>
        f(e) map(gotAcc :+ _)
      }
    }
  }

  def doubleIfOdd(x: Int) = if (x % 2 == 1) Some(x * 2) else None
  assert(traverse(List(1, 3, 5))(doubleIfOdd) == Some(List(2, 6, 10)))
  assert(traverse(List(1, 2, 5))(doubleIfOdd) == None)


  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(identity)
  }

  assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  assert(sequence(List(Some(1), None, Some(3))) == None)
}
