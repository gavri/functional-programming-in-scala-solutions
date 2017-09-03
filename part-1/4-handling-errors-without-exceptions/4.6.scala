object EitherSolution extends App {
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E](error: E) extends Either[E, Nothing] {
    def map[B](f: Nothing => B) = this
    def flatMap[EE >: E, B](f: Nothing => Either[EE, B]) = this
    def orElse[EE >: E, Nothing](b: => Either[EE, Nothing]) = b
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) = this
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    def map[B](f: A => B) = Right(f(value))
    def flatMap[Nothing, B](f: A => Either[Nothing, B]) = f(value)
    def orElse[Nothing, B >: A](b: => Either[Nothing, B]) = this
    def map2[Nothing, B, C](b: Either[Nothing, B])(f: (A, B) => C) = b match {
      case Left(e) => Left(e)
      case Right(gotB) => Right(f(value, gotB))
    }
  }

  assert(Right(2).map(_ + 2) == Right(4))
  assert((Left("Error") : Either[_, Int]).map(_ + 2) == Left("Error"))

  assert(Right(2).flatMap(x => Right(x + 2)) == Right(4))
  assert((Left("Error")).flatMap(x => Right(x)) == Left("Error"))

  assert(Right(2).orElse(Right(3)) == Right(2))
  assert(Left("Error").orElse(Right(3)) == Right(3))

  assert((Right(1).map2(Right(2)))(_ + _) == Right(3))
  assert((Left("Error"): Either[String, Int]).map2(Right(2))(_ + _) == Left("Error"))
  assert(Right(2).map2(Left("Error"))(_ + _) == Left("Error"))
  assert((Left("Error 1"): Either[String, Int]).map2(Left("Error 2"))(_ + _) == Left("Error 1"))
}
