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
    if  (c == 0 || c == r || r == 0)  1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def inner(remainingChar: List[Char], left: Int = 0): Boolean = {
      if (remainingChar.isEmpty) true
      else if (remainingChar.head == ')') {
        if (left <= 0) false
        else {
          inner(remainingChar.tail, left - 1)
        }
      }
      else if (remainingChar.head == '(') {
        inner(remainingChar.tail, left + 1)

      }
      else {
        inner(remainingChar.tail, left)
      }
    }

    inner(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

      def countChange(money:Int, coins: List[Int], count:Int): Int = {
        if (money < 0) count
        else
        if (coins.isEmpty)
          if (money == 0) count + 1 else count
        else
          countChange(money - coins.head, coins, count) + countChange(money, coins.tail, count)
      }
      countChange(money, coins, 0)

  }

}
