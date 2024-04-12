package github.com.imcamilo.datamanipulation

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  /*
  * Are data types in cats that let you keep track of useful information
  *
  * A writer is a wrapper over some valuable value. Second typed parameter
  *
  * */

  import cats.data.Writer

  // 1. Define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("started something"), 29)

  // 2. Manipulate them with pure FP
  val anIncreaseWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "Found something interesting") // value stay the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "Found something interesting", _ + 1) //both, value and logs change
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "Found something interesting", value + 1) //return tuple. both, value and logs change. you can mix them
  }

  // flatMap on Writers
  val writerA = Writer(Vector("Log A1", "Log A2"), 29)
  val writerB = Writer(Vector("Log B1"), 19)

  // logs will be combined by their natural combination function in the presence of a semigroup

  import cats.instances.vector._ // imports a Semigroup[Vector]

  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb // value + value

  // nifty method, reset logs

  import cats.instances.list._ // a Monoid[List[Int]]

  val anEmptyWriter = aWriter.reset // clear the logs and keeps the value

  //3. Dump either the value or logs
  val desireValue = aWriter.value
  val desireLogs = aWriter.written
  val (value, logs) = aWriter.run

  //
  private def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }


  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }

  // BENEFIT #1: Work with pure FP

  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithLogs(n - 1)
      _ <- Writer(Vector(s"Computed  sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n
  }

  // BENEFIT #2: Writers can keep logs separate on multiple threads

  def main(args: Array[String]): Unit = {
    // println(compositeWriter)
    // println(anEmptyWriter)
    // countAndSay(10)
    //countAndLog(10).written.foreach(println)
    implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    Future(naiveSum(100)).foreach(println) // u dont know which thread print
    Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(sumWithLogs(100))
    val sumFuture2 = Future(sumWithLogs(100))
    val logs1 = sumFuture1.map(_.written) // logs for thread 1
    val logs2 = sumFuture2.map(_.written) // logs for thread 2
    //sumWithLogs(100).written.foreach(println)
  }

}
