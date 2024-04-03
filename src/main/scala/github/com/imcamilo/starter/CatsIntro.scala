package github.com.imcamilo.starter

object CatsIntro {

  // valid expression, but is always false... why compiles ? thats why Eq exists
  // val aComparison = 2 == "a String"

  // Eq

  //1. type class import

  import cats.Eq

  //2. type class instance for the type u need
  import cats.instances.int._

  //3. use the type class API
  val intQuality = Eq[Int] //automatically fetches the Eq[Int] instance
  val aTypeSafeComparison = intQuality.eqv(2, 4) //false
  //val anUnsafeComparison = intQuality.eqv(121, "") // doesn't compile

  //4.lets start fancy with extension methods (if u need o if it applicable)

  import cats.syntax.eq._ // all the extensions that eq supports

  val anotherTypeSafeComparison = 2 === 3
  val notEqualComparison = 2 =!= 3
  // val invalidComparison = 2 =!= "3" // doesn't compile

  //5. extending tc operations with composite types. Like lists

  import cats.instances.list._ // we bring Eq[List[Int]] in scope ()

  // but even without the import the compiler infers the necessary instance
  val listComparison = List(2) === List(3) // false

  // So extensions methods are only visible in the presence of the right tc instance

  //6. What if the type is not supported...
  // create tc for custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar]((car1, car2) => car1.price == car2.price)
  val compareTwoToyCars = ToyCar("Ferrari", 452.2) === ToyCar("Lambo", 452.2)


  def main(args: Array[String]): Unit = {
    println(compareTwoToyCars)
  }

  /*
  * Organization
  *     most important are TC
  *     1. use the type class API
  *       import cats.YourTypeClass
  *     2. bring the implicit TC for supported type in scope
  *       import cats.instances.yourType._
  *     3. use extension methods
  *       import cats.syntax.yourTypeClass._
  * */
}
