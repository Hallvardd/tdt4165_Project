object Main extends App {

  def thread(body: => Unit): Thread = {
      val t = new Thread {
        override def run() = body
      }
      t.start
      t
    }

  // Write a few transaction examples using Threads

  val first = Main.thread {
    for (i<- 0 until 10) { println(s"${Bank getUniqueId} t1"); Thread.sleep(10) }
  }
  val second = Main.thread {
    for (i<- 0 until 10) { println(s"${Bank getUniqueId} t2"); Thread.sleep(20) }
  }
  val third = Main.thread {
    for (i<- 0 until 10) { println(s"${Bank getUniqueId} t3"); Thread.sleep(10) }
  }
  val fourth = Main.thread {
    for (i<- 0 until 10) { println(s"${Bank getUniqueId} t4"); Thread.sleep(10) }
  }
  first.join(); second.join(); third.join(); fourth.join()

}
