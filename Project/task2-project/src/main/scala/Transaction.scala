import scala.collection.mutable
import exceptions._

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  val transactions: mutable.ArrayBuffer[Transaction] = mutable.ArrayBuffer[Transaction]()

  // Remove and return the first element from the queue
  def pop: Transaction = this.synchronized{
    transactions.remove(0)
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = this.synchronized{
    transactions.isEmpty
  }

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = this.synchronized{
    transactions += t
  }

  // Return the first element from the queue without removing it
  def peek: Transaction = this.synchronized{
    transactions.head
  }

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = this.synchronized{
    transactions.toIterator
  }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttempts: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  var attempts: Int = 0


  override def run(): Unit = {

    def doTransaction(): Unit = {
      from withdraw amount
      to deposit amount
    }

    try {

      if (from.uid < to.uid) from synchronized {
        to synchronized {
          doTransaction()
        }
      } else to synchronized {
        from synchronized {
          doTransaction()
        }
      }
      this.status = TransactionStatus.SUCCESS
      processedTransactions.push(this)
    } catch {
      case illegalAmount: IllegalAmountException => {
        /* If an illegalAmount (bellow zero) is entered, then the transaction is invalid and is
         labeled failed and put in the processed transactions que.
        */
        this.status = TransactionStatus.FAILED
        processedTransactions.push(this)
      }
      case noSufficientFunds: NoSufficientFundsException => {
        /* If no sufficient funds are available then the transaction is put on hold and
         is attempted the number of times specified in the transaction. When the number
         of attempts is reached the transaction has failed.
        **/
        this.attempts += 1
        if (this.attempts >= this.allowedAttempts) {
          this.status = TransactionStatus.FAILED
          processedTransactions.push(this)

        }
        else {
          transactionsQueue.push(this)
        }
      }

    }
  }
}
