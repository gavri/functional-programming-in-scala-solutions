class Par[A] {
}

object Par {
  def unit[A](a: A): Par[A] = null
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = null
  def fork[A](a: => Par[A]): Par[A] = null
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => {
    lazyUnit(f(a))
  }
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]())) { (e, acc) =>
      map2(e, acc)(_ :: _)
    }
  }
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    map(sequence(as.map { a =>
      asyncF((a: A) => if (f(a)) Some(a) else None)(a)
    }))(_.flatten)
  }
}
