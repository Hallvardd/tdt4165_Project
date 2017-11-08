import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {

    def getBalance: Double = {
      amount
    }

    def setBalance(tran_amount: Double): Unit = {
      amount += tran_amount
    }
  }

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    // Should return a list of all Transaction-objects stored in transactions
    transactions.values.toList
  }

  def allTransactionsCompleted: Boolean = {
    // Should return whether all Transaction-objects in transactions are completed
    for(t <- getTransactions if !t.isCompleted){
      //println(s"${t.from}, ${t.to}, ${t.status}, ${t.isCompleted} ${getFullAddress} ${getBalanceAmount}")
      return false
    }
    true
  }

  def setBalance(amount: Double): Unit = this.synchronized {
    //This method is synchronized, hindering the variable balance from being altered by the two methods accessing the same variable.
    if(balance.getBalance + amount < 0){
      throw new NoSufficientFundsException(s"Amount: $amount, greater than balance: $balance")
    }
    balance.setBalance(amount)

  }

  def withdraw(amount: Double): Unit = {

    if(amount < 0){
      throw new IllegalAmountException(s"Amount: $amount, less than 0")
    }
    setBalance(-amount)
  }

  def deposit(amount: Double): Unit ={
    if(amount < 0){
      throw new IllegalAmountException(s"Amount: $amount, less than 0")
    }
    setBalance(amount)
  }

  def getBalanceAmount: Double = {
    balance.getBalance
  }


  def sendTransactionToBank(t: Transaction): Unit = {
    // Should send a message containing t to the bank of this account
    BankManager.findBank(bankId) ! t
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {

    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)

      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
      }
    }

    t

  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      true
    }

    else{
      false
    }
  }

  override def receive = {
    case IdentifyActor => sender ! this
    case TransactionRequestReceipt(to, transactionId, transaction) => {
      transaction.receiptReceived = true
      transactions += (transactionId -> transaction)
      if(transaction.status == TransactionStatus.FAILED){
        deposit(transaction.amount)
      }
    }

    case BalanceRequest => sender ! getBalanceAmount

    case t: Transaction => {
      // Handle incoming transaction
      deposit(t.amount)
      t.status = TransactionStatus.SUCCESS
      sender ! TransactionRequestReceipt(t.from, t.id, t)

    }

    case msg => println("Hello! This is a nice message, for nice people.")
  }


}
