
object Main extends App {

  var finished: Boolean = false

  def thread(body: =>Unit): Thread = {
      val t = new Thread {
        override def run(): Unit = body
      }
      t.start()
      t
    }

}