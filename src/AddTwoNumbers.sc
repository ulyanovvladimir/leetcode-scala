class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x

  override def toString = if (next == null) x.toString else x.toString + " -> " + _next.toString
}

object Solution {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

    def modDiv(extra: Int, list1: ListNode, list2: ListNode) = {
      val sum = extra + (
        if (list1 == null && list2 == null) 0
        else if (list1 != null && list2 == null) list1.x
        else if (list2 != null && list1 == null) list2.x
        else list1.x + list2.x
        )
      (sum % 10, sum / 10)
    }

    def next(list: ListNode): ListNode = if (list == null) null else list.next

    def tail(extra: Int, list1: ListNode, list2: ListNode): ListNode = {
      val (x, e) = modDiv(extra, list1, list2)
      if (x == 0 && list1 == null && list2 == null) null else new ListNode(x, tail(e, next(list1), next(list2)))
    }

    tail(0, l1, l2)
  }
}