object Solution63 extends App {
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, nextRNG) = rng.nextInt
    val (d, finalRNG) = Solution62.double(nextRNG)
    ((i, d), finalRNG)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (id, finalRNG) = intDouble(rng)
    ((id._2, id._1), finalRNG)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (first, firstRNG) = Solution62.double(rng)
    val (second, secondRNG) = Solution62.double(firstRNG)
    val (third, finalRNG) = Solution62.double(secondRNG)
    ((first, second, third), finalRNG)
  }
}
