package recfun

import scala.collection.mutable.Stack


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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0) 1
    else if (c >= r) 1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def processChars(chars: List[Char], stack: Stack[Char]): Boolean = {
      if (chars.isEmpty) {
        return stack.isEmpty
      }

      val currentChar = chars.head

      if (currentChar == '(') {
        stack.push(currentChar)
      }
      else if (currentChar == ')') {
        if (stack.isEmpty)
          return false
        else if (stack.pop != '(')
          return false

      }

      processChars(chars.tail, stack)
    }

    val stack = new Stack[Char]

    processChars(chars, stack)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money < 0)
      return 0
    else if (money > 0 && coins.isEmpty)
      return 0
    else if (money == 0)
      return 1
    else
      countChange(money, coins.tail) + countChange(money-coins.head, coins)
  }
}
