package github.com.imcamilo.abstractions

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

/*
* https://typelevel.org/cats/datatypes/optiont.html
* https://typelevel.org/cats/datatypes/eithert.html
* */
object MonadTransformers {

  // need unwrap every single option
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT //comes from option transformer
  import cats.instances.future._ //fetch an implicit Option[Future]
  import cats.instances.list._ //fetch an implicit Option[List]

  // wrapping a list of options into a higher kinded OptionT type
  val listOfNumOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  // combine them... without unwrap one by one
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    car <- listOfCharOptions
    num <- listOfNumOptions
  } yield (num, car)
  // That's what a monad transformer does

  // Either transformer

  import cats.data.EitherT

  val listOfEither: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(29), Right(30)))
  // Monads transformers are not limited to List
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither1: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Right(39)))
  val futureOfEither2: EitherT[Future, String, Int] = EitherT.right(Future(39))

  //
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170,
  )
  type AsyncReponse[T] = EitherT[Future, String, T] // Wrapper over Future[Either[String, T]]

  def getBandWidth(server: String): AsyncReponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncReponse[Boolean] = {
    for {
      r1 <- getBandWidth(s1)
      r2 <- getBandWidth(s2)
    } yield r1 + r2 > 250
  } // Future[Either[String, Boolean]]

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncReponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 cannot cope with the incoming spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 cannot cope with the incoming spike: Not enough total bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the incoming spike: No problem")
    }
  // Future[Either[String, Boolean]] -- transform to -- Future[Either[String, String]]
  //


  /*
  * Monad Transformers are Higher Kinded Types for conveniencie over nested Monadic Values
  * */
  def main(args: Array[String]): Unit = {
    println(listOfTuples)
    println(listOfTuples.value)
    val resultFuture = generateTrafficSpikeReport("server2.rockthejvm.com", "server3.rockthejvm.com").value
    resultFuture.foreach(println)
  }

}
