package github.com.imcamilo.datamanipulation

/*
* Mechanism by which an expression is reduced to a value
* */
object Evaluation {

  /*
  * Cats makes the distinction between:
  * - evaluating an expression eagerly
  * - evaluating lazily and every time you request it
  * - evaluating lazily and keeping the value (memoizing)
  * */

  // Eval -> Wrapper over some value

  import cats.Eval

  /*
  * The wrapper has the number already store in the Eval,
  * before I even get the chance to access it
  * Evaluate eagerly
  */

  val instantEval: Eval[Int] = Eval.now {
    println("computing now")
    4231
  }

  /*
  * This expression is not evaluated unless I request it
  * The expression passed to always, will be evaluated every time you access it
  */
  val redoEval = Eval.always {
    println("computing again")
    4321
  }

  /*
  * This expression will be evaluated when its called
  * But if its called twice, the value inside will be kept.
  * Will not evaluate the expression again, just the result. Memoized
  */
  val delayedEval = Eval.later {
    println("computing later")
    4321
  }

  val composeEvaluation: Eval[Int] = instantEval
    .flatMap(instantValue => delayedEval.map(delayedValue =>
      delayedValue + instantValue))

  val composedEvalFor: Eval[Int] = for {
    insValue <- instantEval
    delValue <- delayedEval
  } yield insValue + delValue

  //
  val evalEx1: Eval[Int] = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // remember a computed value
  val dontRecompute = redoEval.memoize

  /*
  the first 2 expressions are evaluated only once
  but the last map expression is evaluated every time
  * */
  val tutorial = Eval
    .always {
      println("Step 1...")
      "put the guitar on your lap"
    }.map { step1 =>
      println("Step 2")
      s"$step1 then put your left hand on the neck"
    }.memoize // remember the value up to this point
    .map { step12 =>
      println("Step 3 more complicated")
      s"$step12 then with the right hand strike the strings"
    }

  // write the execution without run side effects
  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  // rewrite
  def reverseList[T](list: List[T]): List[T] = {
    if (list.isEmpty) list
    else reverseList(list.tail :+ list.head)
  }

  /*but its stack recursive...*/
  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else reverseEval(list.tail).map(_ :+ list.head)

  /*its safe, and tail recursive. Using defer()*/
  def reverseEvalBetter[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEvalBetter(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    println(defer(Eval.now {
      println("now")
      4321
    }).value)
  }

  println(reverseEval((1 to 100).toList).value)

}
