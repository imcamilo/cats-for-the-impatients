package github.com.imcamilo.abstractions

import cats.PartialOrder

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

    /**/
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
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

  // Extension methods for monad comes from a weird imports, them will be clear later, related to cats hierarchy
  // extension methods in Monads are: pure - flatMap (fundamental methods on monads) two diff imports
  //pure
  import cats.syntax.applicative._ // weaker monad and pure is here
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used => Some(1)
  val oneList = 1.pure[List] // List(1)
  //flatMap
  import cats.syntax.flatMap._ //for now is not necessay bcs option has a flatMap method, but,
  // when the data structure doesnt have the flatMap method, this become important
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  /*Monads are Functors. Monad extends Functors.. So*/
  val oneOptionMap = Monad[Option].map(Option(2))(_ + 2)
  import cats.syntax.functor._ // flatmap and map are here
  val oneOptionMap2 = oneOption.map(_ + 2) //map is already present on option

  /*if we have map and flatMap, we have access to for comprehensions*/
  val composedOption = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  /* Shorter version of getPairs using for-comprehension*/

  def getPairsFor[M[_] : Monad, A, B](m: M[A], mb: M[B]): M[(A,B)] = {
    for {
      a <- m
      b <- mb
    } yield (a, b) // same m.flatMap(a => mb.map(b => (a, b)))
  }

  def main(args: Array[String]): Unit = {
    // println(listExpected)
    // println(optionExpected)
    // println(futureExpected)
    // println(aTransformedList)
    // println(aTransformedFuture)
    // println(getPairs(listNums, listChar))
    // println(getPairs(optionNum, optionChar))
    // getPairs(futureNum, futureChar).foreach(println)
    println(getPairsFor(listNums, listChar))
    println(getPairsFor(optionNum, optionChar))
    getPairsFor(futureNum, futureChar).foreach(println)
  }

  /*
  * Monad. Higher Kinded Type Class that Provides:
  *   a pure method. To wrap a normal value into a monadic value.
  *   a flatMap method. To transform monadic values in sequence
  *
  * Also can implement map in terms of pure + flatMap
  *   Monads are natural extension of Functors
  *
  * import cats.syntax.applicative._ // adds the pure extension method
  * val oneValid = 1.pure[ErrorOr]
  * import cats.syntax.functor._ // adds the map extension method
  * val twoValid = oneValid.map(_ + 1)
  * import cats.syntax.flatmap._ // adds the flatMap extension method
  * val transformedValue = oneValid.flatMap(x => (x + 1).purep[ErrorOr])
  *
  * So map + flatMaps = for-comprehensions
  * val composedErrorOr = for {
  *   one <- oneValid
  *   two <- twoValid
  * } yield one + two
  *
  * Use cases:
  * - Sequential transformations:
  *     - List Combinations
  *     - Option Transformations
  *     - Asynchronous chained computations
  *     - Dependent computations
  *
  *
  * For-comprehensions are NOT ITERATIONS
  * Step away from the concept of iteration.
  * FlatMap is a mental model of chained transformations
  * */

}
