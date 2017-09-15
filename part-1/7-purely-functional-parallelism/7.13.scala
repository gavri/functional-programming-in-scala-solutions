import java.util.concurrent.ExecutorService
import scala.concurrent._
import scala.concurrent.duration._

object Solution713 {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = null
    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = null
    def fork[A](a: => Par[A]): Par[A] = null
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a = Await.result(run(es)(pa), 10 seconds)
    choices(a)(es)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices(_))

  def choice[A](parCond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(parCond) { cond =>
    if () t else f
  }

}

