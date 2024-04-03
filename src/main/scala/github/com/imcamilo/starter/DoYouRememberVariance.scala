package github.com.imcamilo.starter

object DoYouRememberVariance {

  import cats.Eq // tc def
  import cats.instances.int._ // Eq[Int] tc instance
  import cats.instances.option._ // Eq[Option[Int]] tc instance
  import cats.syntax.eq._ // extension

  val comp = Option(1) === Option(2)
  /*
  * careful
  * val invalidComparison = Some(2) === None
  * Eq[Some[[Int]] not found. Even though Some its a subtype of Option...
  * In this case
  *   Eq[Some[[Int]] is not a subtype of Eq[Option[[Int]]
  * related to variance.
  * */

  // check
  class Animal

  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+C]

  val cage: Cage[Animal] = new Cage[Cat] //Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated back
  class Vet[-C]

  val vet: Vet[Cat] = new Vet[Animal] // Vet <: Animal, then Vet[Animal] <: Vet[Cat]

  /*
  * Rules
  *
  * If the G generic type has T. "Has a T" = Covariant
  * If the G acts on T. "Operates on T" = Contravariant
  *
  * Variance affects how TC instances are being fetched
  *
  * */

  //acts on T - CONTRAVARIANT TC
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")

  makeSound[Animal] //ok - tc instance defined above
  makeSound[Cat] //ok - tc instance for Animal is also applicable to Cats
  // RULE: Contravariant TCs can use the superclass instances if nothing is available strictly for that type

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]]
  /*
  * If the TC is contravariant, it can support both. Option and Some by the same TC instance
  *
  * If Eq were contravariant I would be able to say Some(1) === None.
  * But Eq is not contravariant.
  * Is invariant (No variant annotation)
  *
  */

  //has T - COVARIANT TC
  trait AnimalShow[+A] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats"
  }

  def organizeShow[A](implicit event: AnimalShow[A]): String = event.show

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // compiler wil inject CatsShow as implicit
    // println(organizeShow[Animal]) // wont compile - compiler found 2 potential implicits - ambiguous values
  }

  /*
  * RULE: Covariant TCs will use the more specific TC instances for that type
  * but may confuse the compiler if the general TC is also present.
  *
  * Covariant type classes will always prefer the more specific TC instances.
  *
  * RULE: You cant have both.
  *
  * Cats uses invariant Type Classes
  *
  * That's why we can't compile
  *   Some(2) === None
  * Instead we use
  *   Option(1) === Option(3)
  *   Option(1) === Option.empty[Int]
  * */

  val okComp = Option(312) === Option.empty[Int]

}
