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
    def pascal(c: Int, r: Int): Int = {
      c match {
        case 0 => 1
        case `r` => 1
        case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def parathesisBalance(count : Int, index : Int) : Boolean = {
        if(index == chars.length) {
          count == 0
        }else {
          chars(index) match {
            case '(' => parathesisBalance(count + 1, index + 1)
            case ')' => if (count <= 0) false else parathesisBalance(count - 1, index + 1)
            case _ => parathesisBalance(count, index + 1)
          }
        }
      }
      parathesisBalance(0, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val sortedCoins = coins.sorted

      def countChangesRec(money : Int, index : Int) : Int = {
        if(money == 0)
           1
        else if(index == coins.length)
           0
        else if(money >= sortedCoins(index)){
          countChangesRec(money - sortedCoins(index), index) +
            countChangesRec(money , index + 1)
        } else  0
      }

      countChangesRec(money , 0)
    }
  }
