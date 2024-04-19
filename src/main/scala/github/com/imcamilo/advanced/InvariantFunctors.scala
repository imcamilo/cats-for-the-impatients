package github.com.imcamilo.advanced

import cats.kernel.Monoid

object InvariantFunctors {


  /*Crypto describes a data structure that can turn a value of type A
  * to an String and back*/

  trait Crypto[A] {
    self =>
    def encrypt(value: A): String

    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)

  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCipher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  /*
  * support for int, double, option[string]
  * */
  implicit val doubleCrypto: Crypto[Double] = caesarCipher.imap(_.toString, _.toDouble)
  implicit val optStrCrypto: Crypto[Option[String]] = caesarCipher.imap(_.getOrElse(""), Option(_))

  // generalizing

  import cats.instances.double._

  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  // cats

  import cats.Invariant
  import cats.Show
  import cats.instances.string._ // Show[String]

  val showStrings: Show[String] = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showStrings)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._

  val showOptionStringShorter = showStrings.imap(Option(_))(_.getOrElse("")) // identical

  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContrariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // "covariant" functor
    def map[A, B](wa: W[A])(forth: A => B): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)
  }

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("let's encrypt")
    val decrypted = decrypt[String](encrypted)
    println(encrypted)
    println(decrypted)
    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))
    println(encrypt(Option("let's encrypt")))
    println(decrypt[Option[String]](encrypted)) // Some('let's encrypt')

    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Option(Math.PI))))

  }

}
