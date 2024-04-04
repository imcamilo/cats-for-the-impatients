package github.com.imcamilo.abstractions

import scala.util.Try

//https://typelevel.org/cats/typeclasses/functor.html
object Functors {

  /*
  * Functor is a Type Class that provides a map method
  * Similar one that you know from lists, options, try.
  * */

  val commonModifiedList = List(1, 2, 3).map(_ + 31) // List(32,33,34)
  val commonModifiedOption = Option(2).map(_ + 31) // Some(33)
  val commonModifiedTry = Try(2).map(_ + 31) // Success(33)

  /*
  * Functors are type class that will generalize the idea of a map function
  * higher kinded type, and map receive 2 type args
  * F can be List, Option, Try, Future
  * Simplified definition
  * */
  trait MyFunctor[F[_]] {
    // in the case of List[Int] we will transform it into a List[?] of something else
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats functor

  import cats.Functor // tc def
  import cats.instances.list._ // tc instance... - includes Functor[List]
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 31) // List(32,33,34)

  import cats.instances.option._ // includes Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 31) // Some(3)

  import cats.instances.try_._
  val tryFunctor = Functor[Try]
  val incrementedTry = tryFunctor.map(Try(2))(_ + 1)

  /*
  * Looks very complicated... more than standar Scala collections API
  * list.map(function) looks less complicated than functor.map(list)(function)
  * So, why do we need Functor?
  *
  * Functors become important when we want to generalize a transformation
  * */

  //Generalizing an API *   multiplying elements in a list by 10 - same implementation - completely different signature
  def doX10List(entry: List[Int]): List[Int] = entry.map(_ * 10)
  def doX10Option(entry: Option[Int]): Option[Int] = entry.map(_ * 10)
  def doX10Try(entry: Try[Int]): Try[Int] = entry.map(_ * 10)
  // so on and so forth

  // so we can define... and generalize
  // def do10x[F[_]](container: F[Int]): F[Int] = ???
  // def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = ???
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // uf

  def main(args: Array[String]): Unit = {
    println(do10x(List(1,2,3)))
    println(do10x(Option(2)))
    println(do10x(Try(2)))
  }

}

