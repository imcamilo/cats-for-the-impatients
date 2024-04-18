package github.com.imcamilo.moretypeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  /*
  * Traverse is a Higher Kinded Type Class with "inside-out" functions
  *
  * Using foldables we work with accumulators using its API
  *
  * Traversable offers a High level Approach to this
  * so called iterations.
  *
  * Traverse is very usefull for turning nested data structures inside out
  * */

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server-ci.imcamilo.com", "server-staging.imcamilo.com", "server-prod.imcamilo.com")

  def getBandwidth(host: String): Future[Int] = Future(host.length * 80)

  /*
  * We have a list of strings,
  * Converts each String to a Future[Int] (each element is turn to independent future)
  * We want Future[List[Int]]
  *
  * IT works but we have to create futures every time, and so on
  * */
  val allBandwidth: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostName) =>
    val bandFuture: Future[Int] = getBandwidth(hostName)
    for {
      accBandwidth <- accumulator
      band <- bandFuture
    } yield accBandwidth :+ band
  }

  // traversing - more concise
  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)

  // List[Future[Int]] -> Future[List[Int]]
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  //

  import cats.Applicative
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  import cats.syntax.apply._ // mapN

  // it could have been a monad, but a weaker monad its ok, Applicative
  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wAcccumulator: F[List[B]], element: A) =>
      val wrapperElement: F[B] = func(element)
      (wAcccumulator, wrapperElement).mapN(_ :+ _)
    }

  //
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    //listTraverse(list)(x => x)
    listTraverse(list)(identity)


  //

  import cats.instances.vector._

  val allPairs = listSequence(List(Vector(1, 2), Vector(3, 4))) // Vector[List[Int]] = all the possible 2-tuples
  val allTriples = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // Vector[List[Int]] - all the possible 3-tuples

  //

  import cats.instances.option._

  // equilvalent to forall returning an Option
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))

  val allTrue = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2, 4, 6))
  val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  //

  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List]

  type ErrorsOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicates for $n failed"))
    }

  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2, 4, 6))
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0) // Invalid(List("predicate for 1", "predicate for 3"))


  // cats

  import cats.Foldable
  import cats.Functor

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B](list: L[A])(func: A => F[B]): F[L[B]]

    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    type Identity[T] = T

    import cats.Id // its the same that Identity

    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._ // implicit Applicative[Future]

  val allBandwidthCats = Traverse[List] //fetches implicit Traverse[List]
  val catsStyle = allBandwidthCats.traverse(servers)(getBandwidth)

  // extension methods

  import cats.syntax.traverse._

  val allBandwidthsCats2 = servers.traverse(getBandwidth) // requires the presences of an Applicative[Future]


  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }

}
