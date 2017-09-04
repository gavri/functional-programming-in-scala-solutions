sealed trait Stream[+A]

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def isEqual[A](left: Stream[A], right: Stream[A]): Boolean = (left, right) match {
    case (Empty, Empty) => true
    case (Empty, _) => false
    case (_, Empty) => false
    case (Cons(leftHead, leftTail), Cons(rightHead, rightTail)) => (leftHead() == rightHead()) && isEqual(leftTail(), rightTail())
  }
}
