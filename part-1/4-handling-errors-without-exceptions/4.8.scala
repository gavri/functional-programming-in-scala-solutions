object BeefedUpMap2Solution extends App {
  trait Either[+E, +A] {
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E](error: E) extends Either[E, Nothing] {
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) = this
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    def map2[Nothing, B, C](b: Either[Nothing, B])(f: (A, B) => C) = b match {
      case Left(e) => Left(e)
      case Right(gotB) => Right(f(value, gotB))
    }
  }

  assert((Right(1).map2(Right(2)))(_ + _) == Right(3))
  assert((Left("Error"): Either[String, Int]).map2(Right(2))(_ + _) == Left("Error"))
  assert(Right(2).map2(Left("Error"))(_ + _) == Left("Error"))
  assert((Left("Error 1"): Either[String, Int]).map2(Left("Error 2"))(_ + _) == Left("Error 1"))
}
