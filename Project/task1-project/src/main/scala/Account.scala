import exceptions.{IllegalAmountException, NoSufficientFundsException}

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId){

  private var balance: Double =  initialBalance

  def setBalance(amount: Double): Unit = this.synchronized {
    //This method is synchronized, hindering the variable balance from being altered by the two methods accessing the same variable.
    if(balance + amount < 0){
      throw new NoSufficientFundsException(s"Amount: $amount, greater than balance: $balance")
    }
    balance = balance + amount
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
    balance
  }

}
