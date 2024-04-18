package github.com.imcamilo.advanced

//https://typelevel.org/cats/datatypes/kleisli.html
object Kleislis {

  /*
  * Kleisli, just a generic data structure,
  * Wrap functions returning F[_] instances.
  *
  * It will help us composing functions returning wrapper instances
  * */

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)
  //val func3 = func2 andThen func1 //doesnt work bcs it needs an int as argument.

  val plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3 = plainFunc2 andThen plainFunc1 //the result of f2 goes to f1, composing regular functions

  //

  import cats.data.Kleisli
  import cats.instances.option._ // implicit FlatMap[Option]

  val func1K: Kleisli[Option, Int, String] = Kleisli(func1) // Kleisli will wrap an option from int to an option string
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2) // Kleisli will wrap an option int to an option int
  // applicable as long as a there is FlatMap[Option] in the scope
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience API
  // multiple results
  val multiply = func2K.map(_ * 2) // Option(...).map(_ * 2) // map is applied to the result of the function
  val chain = func2K.flatMap(x => func1K)

  //
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  val times2 = Kleisli[Id, Int, Int](x => x * 2)
  val plus4 = Kleisli[Id, Int, Int](x => x + 4)
  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  // readers? like dependency injection. Reader is just a Kleisli


  def main(args: Array[String]): Unit = {
    println(composedFor(3))
  }
}
