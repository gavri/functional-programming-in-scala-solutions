 import scala.collection.mutable.ListBuffer

object Solution67 extends App {
  type Rand[+A] = RNG => (A, RNG)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = (rng: RNG) => {
    var currentRNG = rng
    var result = new ListBuffer[A]()
    rs.foreach { r =>
      val singleResult = r(currentRNG)
      result += singleResult._1
      currentRNG = singleResult._2
    }
    (result.toList, currentRNG)
  }

  def ints(n: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(n)((rng: RNG) => rng.nextInt))(rng)
}
