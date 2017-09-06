trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG
{
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Solution64 extends App {
  def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    if (n == 0) {
      (List(), rng)
    }
    else {
      val (nextInt, firstRNG) = rng.nextInt
      val (restInts, secondRNG) = ints(n - 1)(firstRNG)
      (nextInt :: restInts, secondRNG)
    }
  }
}
