package github.com.imcamilo.advanced

import cats.kernel.Monoid

object ContravariantFunctors {

  /**/

  trait Format[T] {
    self =>
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]) = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  /*
  * Problem, given we have Format[MyType], can we also have Format[Option[MyType]]?
  * */

  import cats.instances.option._

  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap(_.getOrElse(m.empty))

  /*
  * IntFormat
  * fo  = Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get)
  * fo2 = Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]])(_.get)
  *
  * fo2 = IntFormat
  *         .contramap[Option[Int]](_.get)
  *         .contramap[Option[Option[Int]]])(_.get)
  *
  *
  * Map applies transformations in sequence
  * Contramap applies transformations in reverse sequence
  *
  * (contravariant Type Class Format) The term has nothing to do with the variance type annotations
  *
  * */

  //cats

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._ // implicit Show[Int]

  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  // extensions

  import cats.syntax.contravariant._

  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Nothing weird"))
    println(format(29))
    println(format(true))
    println(format(Option(43)))
    println(format(Option(Option(43))))
  }

}
