import scala.annotation.tailrec

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x

  override def toString = if (next == null) x + " -> Nil" else x + " -> " + next.toString
}

object Solution {
  def reverseList(head: ListNode): ListNode = {
    @tailrec
    def reverseList(previous: ListNode, current: ListNode): ListNode = {
      val next = current.next
      current.next = previous
      if (next == null) current else reverseList(current, next)
    }

    if (head == null) null else reverseList(null, head)
  }
}

// Scala List with FP approach
def reverseList(list: List[Int]): List[Int] = {
  @tailrec
  def reverseList[T](list: List[T], result: List[T]): List[T] = list match {
    case Nil => result
    case x :: xs => reverseList(xs, x :: result)
  }

  reverseList(list, Nil)
}