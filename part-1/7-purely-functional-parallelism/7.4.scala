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
}
