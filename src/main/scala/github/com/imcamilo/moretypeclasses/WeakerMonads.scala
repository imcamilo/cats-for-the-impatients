package github.com.imcamilo.moretypeclasses

import cats.{Applicative, Apply}


object WeakerMonads {

  /*
  * FlatMap is a weaker monad
  * FlatMap = Apply + flatMap
  * */

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = //fundamental of Apply
      flatMap(wa)(a => map(wf)(f => f(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    //def pure[A](value: A): M[A] //fundamental of Applicative
    //def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] //fundamental of Flatmap
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ //flatMap extension method comes from this import
  import cats.syntax.functor._ //map extension method

  //weaker monad
  def getPairs[M[_] : FlatMap](numbers: M[Int], chars: M[Char]): M[(Int, Char)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

}
