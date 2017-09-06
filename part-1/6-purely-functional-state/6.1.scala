object Solution61 extends App {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    var candidate: Int = 0
    var candidateRng: RNG = null
    do {
      val nextUpdate = rng.nextInt
      candidate = nextUpdate._1
      candidateRng = nextUpdate._2
    } while (candidate == Int.MinValue)
    (Math.abs(candidate), candidateRng)
  }
}
