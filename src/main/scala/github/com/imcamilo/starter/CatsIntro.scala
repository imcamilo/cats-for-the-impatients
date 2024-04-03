package github.com.imcamilo.starter

object CatsIntro {

  //valid expression, but is always false... why compiles ? thats why Eq exists
  val aComparison = 2 == "a String"

  // Eq

  //1. type class import

  import cats.Eq

  //2. type class instance for the type u need
  import cats.instances.int._

  //3. use the type class API
  val intQuality = Eq[Int] //automatically fetches de Eq instance of Int
  val aTypeSafeComparison = intQuality.eqv(2, 4) //false
  //val anUnsafeComparison = intQuality.eqv(121, "") // doesn't compile

  //lets start fancy with extension methods (if u need o if it applicable)

  import cats.syntax.eq._ // all the extensions that eq supports

  val anotherTypeSafeComparison = 2 === 3
  val notEqualComparison = 2 =!= 3
  // val invalidComparison = 2 =!= "3" // doesn't compile

}
