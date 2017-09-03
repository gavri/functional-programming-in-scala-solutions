object Map2Solution extends App {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(gotA), Some(gotB)) => Some(f(gotA, gotB))
      case _ => None
    }
  }

  assert(map2(Some(1), Some(2))(_ + _) == Some(3))
  assert(map2(Option.empty[Int], Some(2))(_ + _) == None)
  assert(map2(Some(1), Option.empty[Int])(_ + _) == None)
  assert(map2(Option.empty[Int], Option.empty[Int])(_ + _) == None)
}
