import exceptions.{IllegalAmountException, NoSufficientFundsException}

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {
  private var balance = initialBalance
  def withdraw(amount: Double): Unit = {
    if(amount < 0){
      throw new IllegalAmountException(s"$amount")
    }
    if(amount > balance){
      throw new NoSufficientFundsException(s"$balance")
    }
    else balance = balance - amount

  }
  def deposit(amount: Double): Unit = {
    if (amount > 0) {
      balance = balance + amount
    }
    else throw new IllegalAmountException(s"$amount")
  }
  def getBalanceAmount: Double = balance
}
