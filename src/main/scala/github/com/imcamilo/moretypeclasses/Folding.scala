package github.com.imcamilo.moretypeclasses

import cats.Eval
import cats.kernel.Monoid

object Folding {

  /*
  * Foldable its a Higher Kinded Type with folding methods
  *
  * In practice its very useful and make you infinite collections not crash
  * */

  // implement all in terms of foldLeft & foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((a, currentList) => f(a) :: currentList)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      //list.foldLeft(List.empty[B])((currentList, a) => currentList ++ f(a))
      list.foldLeft(List.empty[B])((currentList, a) => currentList.foldRight(f(a))(_ :: _))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, currentList) => if (predicate(a)) a :: currentList else currentList)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(Monoid.empty[A])(monoid.combine)
  }

  // Cats has a dedicate type class called Foldable

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]

  val foldableList = Foldable[List]
  val sumLeft = foldableList.foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._ // implicit Foldable[Option]

  val foldableOption = Foldable[Option]
  val sumOption = foldableOption.foldLeft(Option(2), 30)(_ + _)

  // fold right its diff
  // very easily to implement in terms of stack recursion - is stack safe bcs use Eval
  // regardless of the implementation of your container (List(1,2,3))
  val sumRight: Eval[Int] = foldableList.foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  import cats.instances.int._

  val anotherSum = foldableList.combineAll(List(1, 2, 3))

  import cats.instances.string._

  //requires an implicit Monoid[String]. bcs strings will be naturally combined => "123"
  val mappedConcat = foldableList.foldMap(List(1, 2, 3))(_.toString)

  import cats.instances.vector._

  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val traversingNested = (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // extension methods

  import cats.syntax.foldable._

  val sumThree = List(1, 2, 3).combineAll // requires Foldable[List], Monoid[Int] --- combining all
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString) // requires Foldable[List], Monoid[String]


  def main(args: Array[String]): Unit = {
    import ListExercises._
    val numbers = (1 to 10).toList
    println(map(numbers)(_ + 2))
    println(flatMap(numbers)(x => (1 to x).toList))
    println(filter(numbers)(_ % 2 == 0))
    import cats.instances.int._ // Monoid[Int] to scope
    println(combineAll(numbers))
    println(mappedConcat)
  }

}
