object Solution66 {
  type Rand[+A] = RNG => (A, RNG)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng: RNG) => {
    val (a, aRNG) = ra(rng)
    val (b, bRNG) = rb(aRNG)
    (f(a, b), bRNG)
  }
}
