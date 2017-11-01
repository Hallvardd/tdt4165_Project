import java.util.concurrent.atomic.AtomicInteger

object Bank {

  private val idCounter: AtomicInteger = new AtomicInteger(0)

  def transaction(from: Account, to: Account, amount: Double): Unit = {
    from.withdraw(amount)
    to.deposit(amount)
  }

  def getUniqueId: Int = {
    idCounter.addAndGet(1)
  }

}
