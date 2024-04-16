package github.com.imcamilo.moretypeclasses

object Applicatives {

  /*
  * Applicative = Functors + The pure method
  * It has the ability to wrap a normal value into a wrap value
  *
  * Rarely used by themselves but they are applicable to types tha are not quite mondas.
  * But they have the pure and the flatMap functionality
  * */

  import cats.Applicative
  import cats.instances.list._

  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) //List(2)

  import cats.instances.option._ // implicit Applicative[Option]

  val optionApplicative = Applicative[Option]
  val aOption = optionApplicative.pure(2) //List(2)

  // pure extension methods

  import cats.syntax.applicative._

  val aSweetList = 2.pure[List] //List(2)
  val aSweetOption = 2.pure[Option] //List(2)

  // Monad extend Applicative
  // Applicative extend Functor

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // "pure" method
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map method
  val validatedApplicative = Applicative[ErrorsOr]


  //
  //def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ??? // already implemented

  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  // Applicatives has the exact same method: def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  // Applicatives can implement product from Semigroupal
  // Applicative extend Semigroupal

}
