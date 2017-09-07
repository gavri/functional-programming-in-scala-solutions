import scala.collection.mutable.ListBuffer

object Solution610 extends App {
  case class State[S,+A](run: S => (A,S)) {
    def map[B](f: (A) => B): State[S, B] = State((state: S) => {
      val (a, newS) = run(state)
      (f(a), newS)
    })

    def map2[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] = State((state: S) => {
      val (a, aS) = run(state)
      val (b, bS) = that.run(aS)
      (f(a, b), bS)
    })

    def flatMap[B](f: (A) => State[S, B]): State[S, B] = State(state => {
      val (a, aS) = run(state)
      val (b, bS) = f(a).run(aS)
      (b, bS)
    })
  }

  object State {
    def unit[S, A](a: A) = State((state: State[S, A]) => (a, state))
    def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = State((state: S) => {
      var currentState = state
      var result = new ListBuffer[A]()
      ss.foreach { s =>
        val singleResult = s.run(currentState)
        result += singleResult._1
        currentState = singleResult._2
      }
      (result.toList, currentState)
    })
  }
}
