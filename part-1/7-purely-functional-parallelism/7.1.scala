class Par[A] {
}

object Par {
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = null
}
