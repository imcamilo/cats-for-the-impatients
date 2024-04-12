package github.com.imcamilo.datamanipulation

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

  //3. Dump either the value or logs
  val desireValue = aWriter.value
  val desireLogs = aWriter.written
  val (value, logs) = aWriter.run


  def main(args: Array[String]): Unit = {

  }

}
