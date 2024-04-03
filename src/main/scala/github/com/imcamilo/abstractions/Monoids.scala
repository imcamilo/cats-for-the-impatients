package github.com.imcamilo.abstractions

object Monoids {

  /*
  * Important property of the combine method that Semigroup provides is that
  * The combine method is associate
  * https://typelevel.org/cats/typeclasses/semigroup.html#exploiting-laws-associativity
  * |+| is always associative.
  *     if I sum these numbers, starting from left to right or right to left. It will be the same result
  * */

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| extension method

  val numbers = (1 to 1000).toList
  // associativity
  // val sumLeft = numbers.foldLeft(0)(_ + _) 500500
  // val sumRight = numbers.foldRight(0)(_ + _) 500500
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  /* *
  * Define a general API
  * def combineFold[C](list: List[C])(implicit semigroup: Semigroup[C]): C =
  *       list.foldLeft(/* What goes here as starter value? */)(_ |+| _)
  * We need a zero value, a neutral value, Semigroup is not enough.
  * */

  /* MONOID provides an empty value
  * Its the same that semigroup but provides a zero value */

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val strMonoid = Monoid[String]
  val combineInts = intMonoid.combine(23, 6) // 29
  val zero = intMonoid.empty // provides zero value for that type

  import cats.instances.string._ // 0. bring the implicit Monoid[String] to the scope

  val mtString = strMonoid.empty // "".
  val combinedString = strMonoid.combine("I understand ", "Monoids")

  // The fundamental methods of a monoid is .empty

  import cats.instances.option._ // construct of an implicit Monoid[Option[Int]] cause I already have Monoid[Int]

  val optionMonoid = Monoid[Option[Int]]
  val emptyMonoidOption = optionMonoid.empty //None
  val combineMonoidOption = optionMonoid.combine(Option(1), Option.empty[Int]) // Some(2)
  val combineMonoidOption2 = optionMonoid.combine(Option(3), Option(3)) // Some(6)

  // Extension methods for monoid |+|
  // I already have cats.syntax.semigroup._ // for the combine extension method
  // import cats.syntax.monoid._ // it will provide the same extension method |+|
  val combineMonoidOption3 = Option(12) |+| Option(17) //

  // General API using Monoid
  def combineFold0[C](list: List[C])(implicit monoid: Monoid[C]): C = list.foldLeft(monoid.empty)(monoid.combine)

  def combineFold1[C](list: List[C])(implicit monoid: Monoid[C]): C = list.foldLeft(monoid.empty)(_ |+| _)

  // Combine list of phone books as Maps[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647,
    ),
    Map(
      "Charli" -> 372,
      "Daniel" -> 889,
    ),
    Map(
      "Tina" -> 123,
    ),
  )

  import cats.instances.map._

  val massivePhonebook = combineFold1(phonebooks)

  // AGGREGATION WITH MONOIDS
  // shopping cart in different tabs in the browser, refresh and aggregate them into a single one
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid = Monoid.instance[ShoppingCart](
    ShoppingCart(List(), 0.0),
    (sa, sb) => {
      ShoppingCart(sa.items |+| sb.items, sa.total |+| sb.total)
    },
  )

  def checkout(shoppingCart: List[ShoppingCart]): ShoppingCart = {
    combineFold1(shoppingCart)
  }

  val aggregatedShoppingCart = checkout(
    List(
      ShoppingCart(List("item1", "item2"), 1.0),
      ShoppingCart(List("item3"), 2.0),
    )
  )

  def main(args: Array[String]): Unit = {
    println(combineFold1(numbers))
    println(combineFold1(List("checker ", "with monoids")))
    println(massivePhonebook)
    println(aggregatedShoppingCart)
  }

}
