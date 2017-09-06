object Solution68 {
  type Rand[+A] = RNG => (A, RNG)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(Solution61.nonNegativeInt) { candidate =>
      val mod = candidate % n
      if (candidate + (n-1) - mod >= 0)
        RNG.unit(candidate)
      else
        nonNegativeLessThan(n)
    }
  }
}
