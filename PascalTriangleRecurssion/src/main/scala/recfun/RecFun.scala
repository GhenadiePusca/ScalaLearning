package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], opened: Int): Boolean = chars match {
      case Nil => opened == 0
      case '(' :: tail => balanceIter(tail, opened + 1)
      case ')' :: _ if opened < 1 => false
      case ')' :: tail => balanceIter(tail, opened - 1)
      case _ :: tail => balanceIter(tail, opened)
      }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def iter(amount: Int, coins: List[Int]): Int = {
      if (amount == 0) 1
      else if (amount < 0 || coins.isEmpty) 0
      else iter(amount - coins.head, coins) + iter(amount, coins.tail)
    }

    iter(money, coins)
  }
}
