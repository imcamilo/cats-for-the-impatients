package github.com.imcamilo.datamanipulation

/*
* In cats State is a data structure that describes the evolution of a system
* https://typelevel.org/cats/datatypes/state.html
* */
object FunctionalState {

  // S is the type for we call state
  // A is the answer after a single computation
  // from a state S, we obtain a another state plus the result of a single computation
  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value

  // State is an abstraction for iterative computations... b.e.g
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied by 5, obtained $a"

  // Pure FP with states
  val firstTransformation: State[Int, String] = State(s => (s + 1, s"Added 1 to 10... obtained ${a + 1}"))
  val secondTransformation: State[Int, String] = State(s => (s * 5, s"Multiplied by 5... obtained ${a * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap {
    firstResult => secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformationFor = for { //identical
    firstRes <- firstTransformation
    secondRes <- secondTransformation
  } yield firstRes + secondRes

  // why not just composite them?
  val func1 = (s: Int) => (s + 1, s"Added 1 to 10... obtained ${a + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied by 5... obtained ${a * 5}")
  // and then... but f1 returns a tuple..
  // the result is deeply nested in a tuple
  val compositeFunctionResult = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  } // and if u apply andThen... youve got a triple nested tuple

  // Thats why we have to use States like that.

  //
  case class ShoppingCart(items: List[String], total: Double)

  // the return type, means that we have a transition
  // between an old shopping cart to another shopping cart and the total amount at that point
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart =>
    (ShoppingCart(item :: cart.items, cart.total + price), cart.total + price)
  }
  /*
  * OK so we can do an aggregation with the state,
  * but the new value, must go from the external source (method)
  * the state keeps the old value as parameter
  * */

  val camiloCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("iPhone", 500)
    _ <- addToCart("Elixir", 200)
    total <- addToCart("Rustic", 300)
  } yield total
  //

  // mental gymnastic...
  // returns a state data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))

  // returns a state data structure that, when run, returns the value of that state and make no changes
  def get[A]: State[A, A] = State((a: A) => (a, a))

  // returns a state data structure that, when run, returns a Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State((a: A) => (value, ()))

  // returns a state data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State((a: A) => (f(a), a))

  // methods available in cats

  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    // println(compositeTransformation.run(10).value)
    // println(compositeTransformationFor.run(10).value)
    // println(compositeFunctionResult(10))
    println(camiloCart.run(ShoppingCart(List(), 0)).value)
  }

}
