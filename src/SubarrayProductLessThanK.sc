object Solution {
  def numSubarrayProductLessThanK(nums: Array[Int], k: Int): Int = {
    /*
  As pattern is monotonically increasing, so we can use sliding window (number are positive), and we can accumulate answer everytime from current valid window :-
  result += end-start+1
  Nice explaination by @willye
  ex nums = [10,5,2,6]:
  If we start at the 0th{10} index, [{10},5,2,6], the number of intervals is obviously 1.
  If we move to the 1st index, the window is now [{10,5},2,6]. The new intervals created are [5] and [10,5], so we add 2.
  Now, expand the window to the 2nd index: [{10,5,2},6]. The new intervals are [2], [5,2], and [10,5,2], so we add 3.
  The pattern should be obvious by now; we add right - left + 1 to the output variable every loop!
  */
    nums.length match {
      case 0 => 0
      case 1 => if (nums(0) < k) 1 else 0
      case _ if k <= 1 => 0
      case len =>
        var result, start, end = 0
        var prod = 1
        for (end <- 0 until len) {
          prod *= nums(end)
          while (prod >= k && start < len) {
            prod /= nums(start)
            start += 1
          }
          result += end - start + 1
        }
        result
    }
  }
}

Solution.numSubarrayProductLessThanK(Array(10, 5, 2), k = 100) == 5
Solution.numSubarrayProductLessThanK(Array(10, 5, 2, 6), k = 100) == 8
Solution.numSubarrayProductLessThanK(Array(1, 2, 3), k = 0) == 0