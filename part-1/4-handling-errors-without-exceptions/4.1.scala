object OptionSolution extends App {
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

  assert(Some(2).map(_ + 2) == Some(4))
  assert((None : Option[Int]).map(_ + 2) == None)

  assert(Some(2).flatMap(x => Some(x)) == Some(2))
  assert((None).flatMap(x => Some(x)) == None)

  assert(Some(2).getOrElse(3) == 2)
  assert(None.getOrElse(3) == 3)

  assert(Some(2).orElse(Some(3)) == Some(2))
  assert(None.orElse(Some(3)) == Some(3))

  assert(Some(2).filter(_ % 2 == 0) == Some(2))
  assert(Some(2).filter(_ % 2 == 1) == None)
  assert((None: Option[Int]).filter(_ % 2 == 1) == None)
}

