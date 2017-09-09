class Par[A] {
}

object Par {
  def unit[A](a: A): Par[A] = null
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = null
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]())) { (e, acc) =>
      map2(e, acc)(_ :: _)
    }
  }
}
