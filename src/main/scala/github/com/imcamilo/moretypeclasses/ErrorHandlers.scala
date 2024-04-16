package github.com.imcamilo.moretypeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object ErrorHandlers {

  /*
  * lvl1: Try/Catch blocks
  * lvl2: Using Try
  * lvl3: Pure FP with Cats
  * */

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]

    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]

    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  //M wrapper the value and E can be any type as an error for us
  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean)
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(29) // Either[String, Int] = Right(29)
  val failure = monadErrorEither.raiseError[Int]("Something went wrong") //Either[String, Int] = Left("Something went wrong")

  // stdlb "recover"
  val handledError: Either[String, Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 23
    case _ => 30
  }
  // stdlb "recoverWith"
  val handlerError2 = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("Something went wrong") // ErrorOr[Int]
  }
  // "filter"
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100) //looks weird, prefer the extension method

  // Try and Future

  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable in this case

  val exeption = new RuntimeException("Really bad")
  val pureTryException = MonadError[Try, Throwable]
  val raiseTryErr: Try[Int] = pureTryException.raiseError(exeption) //Try[Nothing] => Failure(exception)

  import cats.instances.future._

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val pureFutureException = MonadError[Future, Throwable]
  val raiseFutureErr = pureFutureException.raiseError(exeption) // will be completed with a Failure(exception)

  // Applicative => ApplicativeError

  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]

  type ErrorsOr[T] = Validated[List[String], T]

  import cats.ApplicativeError

  val applicateErrorValidated = ApplicativeError[ErrorsOr, List[String]]
  // share: pure, raiseError, handleError, handleErrorWith

  // extension methods

  import cats.syntax.applicative._ // pure is here
  import cats.syntax.applicativeError._ // raiseError, handleErrorWith, handleError are here

  val extendedSuccess = 29.pure[ErrorsOr] //requires implicit Applicative[ErrorsOr, List[String]]
  val extendedError = List("Badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 29
  }

  import cats.syntax.monadError._ // ensure is here

  val testedSuccess = success.ensure("Something bad")(_ > 100)

  def main(args: Array[String]): Unit = {

  }

}
