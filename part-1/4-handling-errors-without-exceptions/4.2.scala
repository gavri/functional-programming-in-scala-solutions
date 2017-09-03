object VarianceSolution extends App {
  trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }

  case object None extends Option[Nothing] {
    def map[B](f: Nothing => B) = None
    def flatMap[B](f: Nothing => Option[B]) = None
    def getOrElse[B](ob: => B) = ob
    def orElse[B](ob: => Option[B]): Option[B] = ob
    def filter(f: Nothing => Boolean): Option[Nothing] = None
  }

  case class Some[A](a: A) extends Option[A] {
    def map[B](f: A => B) = Some(f(a))
    def flatMap[B](f: A => Option[B]) = {
      f(a) match {
        case Some(b) => Some(b)
        case None => None
      }
    }
    def getOrElse[B >: A](ob: => B) = a
    def orElse[B >: A](ob: => Option[B]): Option[B] = Some(a)
    def filter(f: A => Boolean): Option[A] = f(a) match {
      case true => this
      case false => None
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    xs match {
      case Nil => None
      case _ => Some(xs.sum / xs.length)
    }
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { (m) =>
      mean(xs.map { (x) =>
        math.pow(x - m, 2)
      })
    }
  }

  assert(mean(List(0, 1, 3, 4)) == Some(2))
  assert(mean(Nil) == None)

  assert(variance(List(1, 2, 3, 4, 5)) == Some(2))
  assert(variance(List()) == None)
}
