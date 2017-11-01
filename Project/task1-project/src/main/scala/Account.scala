import exceptions.{IllegalAmountException, NoSufficientFundsException}

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {

  private var balance: Double = initialBalance

  def withdraw(amount: Double): Unit = {
    if(amount > balance){
      throw new NoSufficientFundsException(s"Amount: $amount, greater than balance: $balance")
    }
    if(amount < 0){
      throw new IllegalAmountException(s"Amount: $amount, less than 0")
    }
    balance -= amount
  }

  def deposit(amount: Double): Unit ={
    if(amount < 0){
      throw new IllegalAmountException(s"Amount: $amount, less than 0")
    }
    balance += amount
  }

  def getBalanceAmount: Double = {
    balance
  }

}
