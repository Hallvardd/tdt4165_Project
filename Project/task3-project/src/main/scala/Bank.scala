import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  def createAccount(initialBalance: Double): ActorRef = {
    // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
    BankManager.createAccount(accountCounter.addAndGet(1).toString, bankId, initialBalance)
  }

  def findAccount(accountId: String): Option[ActorRef] = {
    // Use BankManager to look up an account with ID accountId
    try {
      Some(BankManager.findAccount(bankId ,accountId))
    }catch {
      case e: NoSuchElementException => None

    }
  }

  def findOtherBank(bankId: String): Option[ActorRef] = {
    // Use BankManager to look up a different bank with ID bankId
    try {
      Some(BankManager.findBank(bankId))
    }catch {
      case e: NoSuchElementException => None
    }

  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance)
    case GetAccountRequest(id) => sender ! findAccount(id)
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)
    case t: TransactionRequestReceipt => processTransactionRequestReceipt(t)

    case msg => println("This does nothing!")
  }

  def processTransaction(t: Transaction): Unit = {
    implicit val timeout: Timeout = new Timeout(5 seconds)
    val isInternal: Boolean = t.to.length <= 4
    val toBankId: String = if (isInternal) bankId else t.to.substring(0, 4)
    val toAccountId: String = if (isInternal) t.to else t.to.substring(4)
    val transactionStatus = t.status

    // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
    // HINT: Make use of the variables that have been defined above.

    if (isInternal || bankId == toBankId){
      findAccount(toAccountId) match {
        case Some(account: ActorRef) => account ! t
        case None =>
          t.status = TransactionStatus.FAILED
          sender ! TransactionRequestReceipt(t.from, t.id, t)
      }
    }
    else{
      findOtherBank(toBankId) match {
        case Some(bank: ActorRef) =>
          bank ! t
        case None =>
          t.status = TransactionStatus.FAILED
          sender ! TransactionRequestReceipt(t.from, t.id, t)
      }
    }
  }

  def processTransactionRequestReceipt(t: TransactionRequestReceipt): Unit = {
    implicit val timeout: Timeout = new Timeout(5 seconds)
    val isInternal: Boolean = t.toAccountNumber.length <= 4
    val toBankId: String = if (isInternal) bankId else t.toAccountNumber.substring(0, 4)
    val toAccountId: String = if (isInternal) t.toAccountNumber else t.toAccountNumber.substring(4)
    val transactionStatus = t.transaction.status

    // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
    // HINT: Make use of the variables that have been defined above.

    if (isInternal){
      findAccount(toAccountId) match {
        case Some(account: ActorRef) => account ! t
        case None => // do nothing
      }
    }
    else{
      findOtherBank(toBankId) match {
        case Some(bank: ActorRef) => bank ! t
        case None => // do nothing
      }
    }
  }
}