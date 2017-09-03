object SequenceSolution extends App {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(Some(List()): Option[List[A]]) { (acc, e) =>
      acc.flatMap { (gotAcc) =>
        e map(gotAcc :+ _)
      }
    }
  }

  assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  assert(sequence(List(Some(1), None, Some(3))) == None)
}
