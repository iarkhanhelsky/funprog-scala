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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (_, 1) => 1
    case (0, _) => 1
    case (`r`, _) => 1
    case (x, y) => pascal(x - 1, y - 1) + pascal(x, y - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def innerBalance(c: List[Char], s: List[Char]): Boolean = {
      c match {
        case '(' :: rest => innerBalance(rest, '(' :: s)
        case ')' :: rest if s.nonEmpty && '(' == s.head => innerBalance(rest, s.tail)
        case ')' :: rest => innerBalance(rest,  ')' :: s)
        case anythingElse :: rest => innerBalance(rest, s)
        case Nil => s.isEmpty
      }
    }

    innerBalance(chars, Nil)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (m, _) if m < 0 => 0
      case (_, Nil) => 0
      case (0, _) => 1
      case (m, c::rest) => countChange(m, rest) + countChange(m - c, c :: rest)
    }
  }
}
