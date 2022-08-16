object Solution {
  def firstMissingPositive(nums: Array[Int]): Int = {
    var i = 0
    while(i < nums.length) {
      val num = nums(i)
      if (1 <= num && num <= nums.length && i != num - 1) {
        val replaced = nums(num - 1)
        if (1 <= replaced && replaced <= nums.length && replaced != num) {
          nums(num - 1) = num
          nums(i) = replaced
        }
        else {
          nums(num - 1) = num
          i += 1
        }
      }
      else {
        i += 1
      }
    }

    i = 0
    while(i < nums.length) {
      if(nums(i) - i != 1) return i + 1
      i += 1
    }

    nums.length + 1
  }
}