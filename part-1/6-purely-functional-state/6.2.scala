object Solution62 extends App {
  def double(rng: RNG): (Double, RNG) = {
    val (nonNegativeInt, nextRNG) = Solution61.nonNegativeInt(rng)
    (nonNegativeInt / Int.MaxValue.toDouble, nextRNG)
  }
}
