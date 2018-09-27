package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == r || c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelp(acc: Int, rest: List[Char]): Boolean =
      if (rest.isEmpty) acc == 0
      else if (acc == 0 && rest.head == ')')
        false
      else if (acc > 0 && rest.head == ')')
        balanceHelp(acc - 1, rest.tail)
      else if (rest.head == '(')
        balanceHelp(acc + 1, rest.tail)
      else
        balanceHelp(acc, rest.tail)

    balanceHelp(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0)
      1
    else if (coins.isEmpty || money < 0) 0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
