package github.com.imcamilo.moretypeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {

  /*
  * Weaker Applicative (Apply) = Functors + Semigroupal + ap method
  *
  * Convenient for extracting and combine tuples
  * */


  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] // fundamental
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental
  }


  import cats.Apply
  import cats.instances.option._ //implicit Apply[Option]

  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._ //extensions methods for Apply

  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuple: Option[(Int, Int, Int)] = tupleOfOptions.tupled // Option(1,2,3)
  val sumN = tupleOfOptions.mapN(_ + _ + _) // Some(6)

  def main(args: Array[String]): Unit = {

  }

}
