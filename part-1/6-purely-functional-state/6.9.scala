object Solution69 {
  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](gen: Rand[A])(f: A => B): Rand[B] = Solution68.flatMap(gen) { (g) => RNG.unit(f(g)) }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = Solution68.flatMap(ra) { a =>
    map(rb) { b => (f(a, b)) }
  }
}
