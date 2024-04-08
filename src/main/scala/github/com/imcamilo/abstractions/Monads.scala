package github.com.imcamilo.abstractions

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

/*
* M word
*
* */
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


  def main(args: Array[String]): Unit = {
    // println(listExpected)
    // println(optionExpected)
    // println(futureExpected)
  }

}
