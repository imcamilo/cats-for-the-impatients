package github.com.imcamilo.abstractions

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

/*
* M word
*
* */
//https://typelevel.org/cats/typeclasses/monad.html
object Monads {

  // lists
  // 1.1 how would u create all the combinations (nums and chars)
  val listNums: List[Int] = List(1, 2, 3)
  val listChar: List[Char] = List('a', 'b', 'c')
  val listExpected: List[(Int, Char)] = listNums.flatMap(n => listChar.map(c => (n, c)))
  val listExpected2: List[(Int, Char)] = for {
    n <- listNums
    c <- listChar
  } yield (n, c) // identical

  //options
  // 1.1 how would u create a combinations for these 2 options
  val optionNum = Option(2)
  val optionChar = Option('d')
  val optionExpected = optionNum.flatMap(n => optionChar.map(c => (n, c)))
  val optionExpected2 = for {
    n <- optionNum
    c <- optionChar
  } yield (n, c)

  //futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureNum = Future(29)
  val futureChar = Future('F')
  val futureExpected = futureNum.flatMap(n => futureChar.map(c => (n, c)))
  val futureExpected2 = for {
    n <- futureNum
    c <- futureChar
  } yield (n, c)

  /**
   * we do have a pattern. We have two fundamental operations.
   *
   * Number 1:
   * The first fundamental operation: Is the ability to wrap a regular value like a number into
   * a wrapper value like a List, Option, Future, or Try.
   * Wrapping a value into a M value
   *
   * Number 2:
   * The FlatMap mechanism
   * Then we have the ability to transform these values with FlatMap.
   * Acts in different ways on different data structures.
   * But the expressions looks the exactly the same
   *
   * (Flatmap is a general transformation, not a iteration,
   * applicable to many diff data, not just sequential data structure like list, bcs future runs in diff thread)
   * But flatMap does guarantee a sequential order of evaluation
   *
   * The Cats type class.
   * Embody these two capabilities is called Monad.
   *
   * Monad
   *
   * Its higher-kinded much like Functors
   *
   */

  /**
   * It needs fulfill the pattern:
   * - Wrapping the value into a M value
   * - The flatMap mechanism
   */
  trait MyMonad[M[_]] {
    /**
     * So M is List
     * and A is an Int
     * We have to return M[A] => List[Int] */
    def pure[A](value: A): M[A] //Wrapping the value into a monadic value

    /**
     * FlapMap has a very similar signature that Functor. Just the function is dff */
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  // cats monad

  import cats.Monad
  import cats.instances.option._ //implicit Monad[Option]

  // Option
  val optionMonad = Monad[Option]
  //capability wrap the value into a M value
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  //capability to transform into other monad
  val transformOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  // List

  import cats.instances.list._ //implicit Monad[List]

  val listMonad = Monad[List]
  val aList = listMonad.pure(3) //List(3)
  // the function takes an element of type Int and return a list of something else (maybe)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) //List(3, 4)

  // Future

  import cats.instances.future._

  val futureMonad = Monad[Future] // require an implicit ec contxt
  val aFuture = futureMonad.pure(30)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(a => Future(a - 1)) //Future will end up with a Success(29)

  /**
   * So we have a general pattern for pure and flatMap for a variety of data structures
   *
   * Why is this useful? This is useful for general APIs
   * */

  // Specialized API
  def getPairsList(nums: List[Int], chars: List[Char]): List[(Int, Char)] = nums.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(num: Option[Int], char: Option[Char]): Option[(Int, Char)] = num.flatMap(n => char.map(c => (n, c)))

  def getPairsFuture(num: Future[Int], char: Future[Char]): Future[(Int, Char)] = num.flatMap(n => char.map(c => (n, c)))

  /** Generalize
   * We can flatMap any kind of DataStructure,  as long as you have an implicit monad in scope
   *
   * Powerful bcs you could transform any kind of monadic value as long as you have an implicit Monad[M] in scope
   */
  def getPairs[M[_], A, B](m: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(m)(a => monad.map(mb)(b => (a, b)))

  def main(args: Array[String]): Unit = {
    // println(listExpected)
    // println(optionExpected)
    // println(futureExpected)
    // println(aTransformedList)
    // println(aTransformedFuture)
    println(getPairs(listNums, listChar))
    println(getPairs(optionNum, optionChar))
    getPairs(futureNum, futureChar).foreach(println)
  }

}
