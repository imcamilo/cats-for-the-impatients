package github.com.imcamilo.moretypeclasses


import cats.Semigroup

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}


object Semigroupals {

  /*
  * Higher Kinded Types class which can tuple elements
  * And the fundamental method of a Semigroupal is the product method
  *
  * Monad extend Semigroupal
  *   The product method can be implemented in terms of maps and flatMaps
  *
  * Some Semigroupals are useful without being Monads
  *   Validated
  *
  * */

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]

  val optionSemigroupal = Semigroupal[Option]
  val aTupleOption = optionSemigroupal.product(Some(123), Some("A string")) // Some((123, "A string"))
  val aNoneTuple = optionSemigroupal.product(Some(123), None) //None

  import cats.instances.future._ //implicit Semigroupal[Future]

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureSemigroupal = Semigroupal[Future] //using Semigroupal[Future]
  val aTupleFuture = futureSemigroupal.product(Future("Years old"), Future(29)) // Future(("Years old", 29))

  // why are useful?

  import cats.instances.list._

  val listSemigroupal = Semigroupal[List]
  val aTupleList = listSemigroupal.product(List(1, 2), List("a", "b"))

  //zip as a fundamental function between these two kind of lists

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    //monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // Monad Extend Semigroupals

  //why?: Validated without the necessity of use Monads and fulfil the monads laws

  // not monadic

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]
  val invalidCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This cant be right"))
  )

  // monadic
  type EitherErrorsOr[T] = Either[List[String], T]

  import cats.instances.either._ // implicit Monad[Either]

  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // implemented in terms of map and flatMap
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This cant be right")) // this error is not propagated bcs flatMap shorts circuits
  )

  // Rules on Monads:
  // Associative
  // m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
  // true for Either, false for Validated

  //
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] = listA.zip(listB)
  }

  def main(args: Array[String]): Unit = {
    println(invalidCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(List(1, 2, 3), List("A", "B")))
  }

}
