package recfun

import common.***

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
    *
    * Pascal function takes a column c and a row r, counting from 0
    * and returns the number at that spot in the triangle.
    *
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r || r == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    *
    * Function verifies the balancing of parentheses in a string,
    * which we represent as a List[Char] not a String.
    *
    */
  def balance(chars: List[Char]): Boolean = {

    def skip(chars: List[Char]): List[Char] = {
      if (chars.isEmpty || chars.head == '(' || chars.head == ')') chars
      else skip(chars.tail)
    }

    def loop(acc: Int, chars: List[Char]): Boolean = {
      val new_chars = skip(chars)
      if (acc < 0) false
      else if (new_chars.isEmpty) acc == 0
      else {
        val head = if (new_chars.head == '(') 1 else -1
        loop(acc + head, new_chars.tail)
      }
    }

    loop(0, chars)
  }

  /**
    * Exercise 3
    *
    * Recursive function that counts how many different
    * ways you can make change for an amount,
    * given a list of coin denominations.
    * 
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sorted_coins = coins.sortWith(_ > _)

    def count(coins: List[Int], money: Int): Int = {
      if (coins.isEmpty) 0
      else if (money == 0) 1
      else if (money < 0) 0
      else if (coins.lengthCompare(1) == 0)
        if (money % coins.head == 0) 1
        else 0
      else count(coins.tail, money) + count(coins, money - coins.head)
    }

    count(sorted_coins, money)
  }
}
