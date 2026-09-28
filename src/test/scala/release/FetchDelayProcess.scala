package release

object FetchDelayProcess {
  def main(args: Array[String]): Unit = {
    if (args.headOption.contains("progress")) {
      (1 to 6).foreach { _ =>
        println("data")
        Thread.sleep(200)
      }
    } else {
      Thread.sleep(10_000)
    }
  }
}
