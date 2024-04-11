package github.com.imcamilo.datamanipulation

import cats.Id
import cats.data.Kleisli
import github.com.imcamilo.abstractions.MWord.OrderStatus

/*
* Readers will serve
*
* We have a multi layer application
* - configuration file with initial data structure
* - a DB layer
* - an Http layer
* - a business layer
*
* All this layers are informed by the initial data structure fetch from the configuration file
* The configuration file feeds all the layers in the application
* The idea is to feed the initial data structure to the entire application.
*
* The reader data type will embody this principle
*
* */
object Readers {

  case class Configuration(dbUsername: String, dbPassword: String, host: String,
                           port: Int, nThreads: Int, emailReplyTo: String)

  case class DBConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" //select * from table and return the status for an order id

    def getLastOrderId(username: String): Long = 12345 // max orderId from table where username is username...
  }

  case class HTTPService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  //instead reading from a file... Im defining the config
  val config = Configuration("camilo", "weak_pwd!", "localhost", 1234, 2, "camilo@camilo.com")

  /**
   * now. We can insert a Reader to inform the creation of a db connection and a http service, based on this config
   *
   * Reader is a data processing type for cats that use this pattern
   */

  import cats.data.Reader

  // typed param. 1 Input, 2 Output
  // in this reader we are obtaining a DBConnection
  val dbReader: Reader[Configuration, DBConnection] =
    Reader(config => DBConnection(config.dbUsername, config.dbPassword))
  val dbConn = dbReader.run(config)

  //Reader[I, O]
  val camiloOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val camiloOrderStatus: Id[String] = camiloOrderStatusReader.run(config)

  // Readers are useful for this kind of situations
  def getLastOrderStatus(username: String): String = {
    // just like nested options ... readers support flatMaps
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    val usersOrdersFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    //usersLastOrderIdReader.run(config)
    usersOrdersFor.run(config)
  }
  /*
  * Pattern goes like...
  * 1. You create the initial data structure
  * 2. You create Reader which specifies how that data structure will be manipulated later
  * 3. You can then map and flatMap the reader to produce derived information
  * 4. When you need the final piece of information, you call run on the reader with the initial data structure
  * */

  //
  case class EmailService(emailReplayTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplayTo; to $address >>> $contents"
  }

  val emailServiceReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyTo))

  def emailUser(username: String, userEmail: String): String = {
    // fetch status from last order
    // email them with email service "Your last order has the status: (status)
    val emailFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailSent <- emailServiceReader.map(_.sendEmail(userEmail, s"Your last order hast the status: ($orderStatus)"))
    } yield emailSent

    emailFor.run(config)
  }

  /*
  * That's how you can implement Dependency Injection in a pure functional way with Cats
  * */

  def main(args: Array[String]): Unit = {
    println(camiloOrderStatus)
    println(getLastOrderStatus("camilo"))
    println(emailUser("camilo", "camilongi@gmail.com"))
  }
}
