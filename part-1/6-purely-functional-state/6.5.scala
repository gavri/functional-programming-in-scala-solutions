object Solution65 extends App {
  def double(rng: RNG): (Double, RNG) = {
    RNG.map(Solution61.nonNegativeInt)((x) => x / Int.MaxValue.toDouble)(rng)
  }
}
