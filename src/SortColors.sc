object Solution {
  def sortColors(nums: Array[Int]): Unit = {

    def swap(s: Int, d: Int): Unit = {
      val tmp = nums(d)
      nums(d) = nums(s)
      nums(s) = tmp
    }

    nums.indices.foldLeft(0, 0, nums.length - 1) {
      case ((lo, mid, hi), _) if mid <= hi => nums(mid) match {
        case 0 =>
          // Swap lo and mid value and increment lo and mid pointers.
          swap(lo, mid)
          (lo + 1, mid + 1, hi)
        case 1 =>
          // Increment just mid pointer
          (lo, mid + 1, hi)
        case 2 =>
          // Swap mid and hi values, and decrement hi pointer
          swap(mid, hi)
          (lo, mid, hi - 1)
      }
    }
  }
}

val nums = Array(2, 0, 2, 1, 1, 0)
Solution.sortColors(nums)
nums