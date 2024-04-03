package github.com.imcamilo.abstractions

/*Lets start with the import for every app*/

object Semigroups {

  /*
  * Type class that provides a combination function
  * Semigroups combines elements of the same type
  * */

  import cats.Semigroup // tc
  import cats.instances.int._ // tc instance
  import cats.instances.string._ // tc instance

  val intSemigroup = Semigroup[Int]
  val intCombination = intSemigroup.combine(22, 7) //natural combination is an addition
  val strSemigroup = Semigroup[String]
  val strCombination = strSemigroup.combine("Lets play with", "Cats")
  /*
  * The combination method
  * will do whatever your intuition says!
  * int     => addition
  * string  => concatenation
  *
  * So, the elements are combined without us needing to specify how they should be combined
  *
  * */

  // SPECIFIC API
  def reduceInts(list: List[Int]): Int = list.reduce(intSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(strSemigroup.combine)
  // GENERAL REDUCTION API, GIVEN THE PRESENCE OF A SEMIGROUP[C]
  def reduceAll[C](list: List[C])(implicit semigroup: Semigroup[C]): C = list.reduce(semigroup.combine)
  def reduceAll2[C: Semigroup](list: List[C]): C = list.reduce(implicitly[Semigroup[C]].combine)

  /*
  * For the specific API we can do .sum or .reduce(_ + _)
  * But the power of the semi group is giving us the capability to define very general combination or reduction API
  * Regardless of what type we are combining. Given the fact that we have the presence of a Semigroup[C]
  * */

  val numbers = (1 to 10).toList
  val strs = List("I ", "started ", "to ", "like ", "semigroups")
  import cats.instances.option._ // this will bring Semigroup[Option[_]] as well
  val numberOptions = numbers.map(a => Option(a))
  /*
  * whats a natural combination between two Option[Int]? =>
  * Compiler will produce an implicit Semigroup[Int] - Combine will produce Option[Int] with the summed elements
  * Compiler will produce an implicit Semigroup[String] - Combine will produce Option[String] with the concatenated elements
  * Same for any type with an implicit semigroup
  * */

  val stringOptions = strs.map(a => Option(a))

  // SUPPORTING A NEW TYPE
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance((e1, e2) => {
    Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  })
  val expenses = List(Expense(1, 99), Expense(4, 35), Expense(3, 10))
  val aggregatedExpenses = expenses.reduce(expenseSemigroup.combine)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(strCombination)
    println(reduceInts(numbers))
    println(reduceAll(numbers))
    println(reduceStrings(strs))
    println(reduceAll(strs))
    println(reduceAll(numberOptions)) //an Option[Int] containing the sum of all elements
    println(reduceAll(stringOptions))
    println(reduceAll(expenses))
    /*
    * This is the kind of functionality that a Semigroup give us for free
    * */
  }

}
