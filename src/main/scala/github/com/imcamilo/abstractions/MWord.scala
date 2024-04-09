package github.com.imcamilo.abstractions

import cats.PartialOrder

import scala.util.Try

object MWord {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List] // fetch the implicit Monad[List]
  val simpleList = monadList.pure(29) // List(29)
  val anExtendedList = monadList.flatMap(simpleList)(x => List(x, x + 1))
  // applicable for Option, Try, Future

  /**
   * The left class will wrap an string, o the right class will wrap an int
   * Either is being thought of as having a valuable or desirable value as right-hand type.
   *
   * The desirable value of this either is the int type
   *
   * Either is also a Monad
   */
  val aManualEither: Either[String, Int] = Right(29)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  // we can import cats instances of Monad[Either]

  import cats.instances.either._

  // whatever type alias I define as type, either with the left hand side having a concrete type, and the right hand side
  // having a generic type, cats can also contruct a monad of my type.
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(30) // LoadingOr[Int] == Right(30)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading x number..."))

  // online store case, and tracking status of our orders in the online store
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam. NL")
  // two services that return instantaneous views of your data...
  // If you were to track and order location for a particular orderId, you would need to call both of these APIs and
  // combine them together... The combination of this two apis, are applicable for a Monad

  val orderId = 456L
  // this is how you would combine results or monadic values with Monad of that type in the scope
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))

  // using extensions methods
  // either already have map and flatMap that's why its unused...

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocaltionBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    locatedIn <- trackLocation(orderStatus)
  } yield locatedIn

  // service layer API of a web app
  case class Connection(host: String, port: String)

  val config = Map("host" -> "localhost", "port" -> "4040")

  trait HttpService[M[_]] {
    def getConnection(config: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }
  /* *
  * If the host and the port are found in the configuration map, then we'll return a M containing a connection with those values
  * otherwise the method will fail, according to the logic of the type M
  * (for Try it will return a Failure, for Option it will return None, for Future it will return Failure, for Either it will return Left
  *
  * The issueRequest method returns a M containing the string "request (payload) has been accepted", if the payload is less than 20 chars
  * otherwise the method will fail, according to the logic of the type M
  *
   */


  object OptionHttpService extends HttpService[Option] {
    def getConnection(config: Map[String, String]): Option[Connection] =
      for {
        h <- config.get("host")
        p <- config.get("port")
      } yield Connection(h, p) // if h or p doesnt exists, the method will return None. simple

    def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None
      else Some(s"Request ($payload) has been accepted")
  }

  val responseOption: Option[String] =
    OptionHttpService.getConnection(config)
      .flatMap(conn => OptionHttpService.issueRequest(conn, "Hello from O.Http.s"))

  val responseOptionFor: Option[String] = for {
    conn <- OptionHttpService.getConnection(config)
    request <- OptionHttpService.issueRequest(conn, "Hello, Http service")
  } yield request

  object ErrorOrHttpService extends HttpService[ErrorOr] {
    def getConnection(config: Map[String, String]): ErrorOr[Connection] =
      if (!config.contains("host") || !config.contains("port")) Left(new RuntimeException("Invalid configuration"))
      else Right(Connection(config("host"), config("port")))

    def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Payload is too large"))
      else Right(s"Request ($payload) has been accepted")
  }

  val responseErrorOr: ErrorOr[String] =
    ErrorOrHttpService.getConnection(config).flatMap(conn => ErrorOrHttpService.issueRequest(conn, "Expecting response."))

  val reponseErrorOrFor: ErrorOr[String] = for {
    conn <- ErrorOrHttpService.getConnection(config)
    response <- ErrorOrHttpService.issueRequest(conn, "Expecting response.")
  } yield response

  // doing it but with monads...
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] = {
    // I already have the extensions methods... so
    for {
      connection <- service.getConnection(config)
      response <- service.issueRequest(connection, payload)
    } yield response
  }

  def main(args: Array[String]): Unit = {
    println(getResponse(OptionHttpService, "Hello option"))
    println(getResponse(ErrorOrHttpService, "Hello error or"))
  }

}
