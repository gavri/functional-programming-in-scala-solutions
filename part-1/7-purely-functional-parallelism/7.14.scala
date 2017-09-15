import java.util.concurrent.ExecutorService
import scala.concurrent._
import scala.concurrent.duration._

object Solution714 {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = null
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = null
  def fork[A](a: => Par[A]): Par[A] = null
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def flatMap[A,B](pa: Par[A])(f: A => Par[B]): Par[B] = es => {
    val a = Await.result(run(es)(pa), 10 seconds)
    f(a)(es)
  }

  def join[A](ppa: Par[Par[A]]): Par[A] = es => {
    Await.result(run(es)(ppa), 10 seconds)(es)
  }

  def flatMapThroughJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))

  def joinThroughFlatMap[A](ppa: Par[Par[A]]): Par[A] = flatMap(ppa)(identity)
}
