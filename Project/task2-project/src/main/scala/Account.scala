import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {

    def getBalance: Double = {
      amount
    }

    def setBalance(tran_amount: Double): Unit = {
      amount += tran_amount
    }

  }

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

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


  def transferTo(account: Account, amount: Double) = {
    bank addTransactionToQueue (this, account, amount)
  }

}
