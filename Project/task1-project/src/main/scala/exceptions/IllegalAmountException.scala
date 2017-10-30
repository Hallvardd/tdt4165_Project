package exceptions

class IllegalAmountException (message: String = null, cause: Throwable = null) extends RuntimeException(message, cause) {
  println(s"Illegal amount: $message $cause")
}