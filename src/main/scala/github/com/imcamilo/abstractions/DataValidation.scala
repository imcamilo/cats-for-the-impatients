package github.com.imcamilo.abstractions

import scala.annotation.tailrec
import scala.util.Try

object DataValidation {

  import cats.data.Validated

  //like an Either
  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val aInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value
  // condition function takes 3 arg: predicate, valid, invalid
  val aTest: Validated[String, Int] = Validated.cond(42 > 21, 99, "you have failed")
  // why dont use a Either...

  /*
  * Use Either
  * - n must be a prime
  * - n must be non negative
  * - n <= 100
  * - n must be even
  * */
  def testPrime(n: Int): Boolean = {
    @tailrec def tailRecPrime(d: Int): Boolean = {
      if (d <= 1) true
      else n % d != 0 && tailRecPrime(d - 1)
    }

    if (n == 0 || n == 1 || n == -1) false
    else tailRecPrime(Math.abs(n / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isTooBig: List[String] = if (n >= 100) List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (testPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isTooBig ++ isNotPrime)
  }

  // validating with cats

  import cats.Semigroup
  import cats.instances.list._ // combination function for lists

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max) //semigroup of int

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n >= 100, n, List("Number must be less than or equal to 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))

  // CHAINING
  aValidValue.andThen(_ => aInvalidValue) // if first is invalid, there is no more transformations
  // TEST A VALID VALUE
  // if the predicates succeed, valid value will stay valid, else will turn into invalidad with the List of errors
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // TRANSFORM
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length) // errors
  aValidValue.bimap(_.length, _ + 1) // bi map like writers
  // INTEROPERATE WITH stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(29))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Value is None"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try(29))
  // BACKWARDS
  aValidValue.toOption
  aValidValue.toEither

  // form validation example
  object FormValidation {

    import cats.instances.string._ // for combinations in andThen

    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"Field name: $fieldName must be specified"))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank"))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List(s"Email ($email) is invalid"))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List(s"Password must be at least 10 characters long"))

    /*
    * validate fields:
    * name
    * email
    * password
    *
    * rules:
    * name, email, pwd must be specified
    * name must not be blank
    * email must have "@"
    * password must have >= 10 characters
    */


    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "name").andThen(name => nonBlank(name, "name"))
        .combine(getValue(form, "email").andThen(emailProperForm))
        .combine(getValue(form, "password").andThen(passwordCheck))
        .map(_ => "User registration complete")

  }


  // extensions
  import cats.syntax.validated._
  val currentAge: Validated[List[String], Int] = 29.valid[List[String]]
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    val form = Map(
      "name" -> "Camilo Jorquera",
      "email" -> "camilo.jorquera.a@gmail.com",
      "password" -> "qwerty1234",
    )
    println(FormValidation.validateForm(form))
  }

}
