import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

  private val uid: AtomicInteger = new AtomicInteger(0)
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val pool = new ForkJoinPool()
  private val executorContext: ExecutionContext = {ExecutionContext.fromExecutorService(pool)}
  //private val executorContext: ExecutionContext = ExecutionContext.global


  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)

  }

  def generateAccountId: Int = {
    uid.addAndGet(1)
  }

  private def processTransactions(): Unit = synchronized {
    /* - has problem terminating threads, when running main outside of test
       - task 13 fails more often than not, this can be reduced by not calling
       processTransactions recursively, but this seems counter-intuitive

     */
    val t = Main.thread {
      while (!transactionsQueue.isEmpty) {
        val transaction: Transaction = transactionsQueue.pop
        executorContext.execute(transaction)
      }
      processTransactions()
    }

  }


  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    processedTransactions.iterator.toList
  }

  def getTransactionsAsList: List[Transaction] = {
    transactionsQueue.iterator.toList
  }

  processTransactions()

}
