object Solution {
  def reverseBetween(head: ListNode, left: Int, right: Int): ListNode = {
    def reverse(head: ListNode, k: Int): ListNode = {
      if (head == null) return head
      if (k < left || k >= right) {
        head.next = reverse(head.next, k+1)
        head
      }
      else {
        val tail = reverse(head.next, k+1)
        val s = head.next.next
        head.next.next = head
        head.next = s
        tail
      }
    }
    reverse(head, 1)
  }
}

// FP, standard List
def reverseBetween[T](list: List[T], left: Int, right: Int): List[T] = {
  def reverseBetween[T](list: List[T], pos:Int, reverted: List[T]): List[T] = {
    list match {
      case Nil => Nil
      case x :: xs if (pos < left) => x :: reverseBetween(xs, pos+1, reverted)
      case x :: xs if pos >=left && pos <= right => reverseBetween(xs, pos+1, x :: reverted)
      case _ if (pos>right) => reverted concat list
    }
  }

  reverseBetween(list, 1, Nil)
}

// FP 2, standard List
def reverse[T](list: List[T], start: Int, end: Int): List[T] = {
  list.take(start - 1) concat list.slice(start - 1, end).reverse concat list.drop(end)
}