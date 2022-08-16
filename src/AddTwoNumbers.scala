/*
  Implementation for Scala List
 */

object Solution {
  def addTwoNumbers(l1: List[Int], l2: List[Int]): List[Int] = {

    def addWithCarry(lists: (List[Int], List[Int]), carry: Int) = lists match {
      case (Nil, Nil) => if (carry == 0) Nil else List(carry)
      case (x :: xtail, Nil) => addHeads(x, 0, carry, (xtail, Nil))
      case (Nil, y :: ytail) => addHeads(0, y, carry, (Nil, ytail))
      case (x :: xtail, y :: ytail) => addHeads(x, y, carry, (xtail, ytail))
    }

    def addHeads(x: Int, y: Int, carry: Int, tails: (List[Int], List[Int])): List[Int] = {
      val sum = x + y + carry
      sum % 10 :: addWithCarry(tails, sum / 10)
    }

    addWithCarry((l1, l2), 0)
  }
}
