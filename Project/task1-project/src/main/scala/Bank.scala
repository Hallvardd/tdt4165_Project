import java.util.concurrent.atomic.AtomicInteger

object Bank {

  //private val idCounter: AtomicInteger = new AtomicInteger(0)
  private var idCounter: Int = 0

  def transaction(from: Account, to: Account, amount: Double): Unit = {
    from.withdraw(amount)
    to.deposit(amount)
  }
/*
  def getUniqueId: Int = synchronized{
    idCounter.addAndGet(1)
  }
*/
  def getUniqueId: Int = this.synchronized{
    val freshUid = idCounter + 1
    idCounter = freshUid
    freshUid
  }

}
