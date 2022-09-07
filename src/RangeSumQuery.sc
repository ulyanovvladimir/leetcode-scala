class NumArray(_nums: Array[Int]) {
  private val preSum = _nums.scan(0)(_ + _)

  def sumRange(left: Int, right: Int): Int = {
    preSum(right + 1) - preSum(left)
  }

  /*
    Without presum Scala one-liner
   */
  def sumRange2(left: Int, right: Int): Int = {
    _nums.slice(left, left + right - left + 1).sum
  }

}

val numArray = new NumArray(Array(-2, 0, 3, -5, 2, -1))
numArray.sumRange(0, 2) == 1
numArray.sumRange(2, 5) == -1
numArray.sumRange(0, 5) == -3
