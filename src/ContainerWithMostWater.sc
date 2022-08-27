object Solution {
  def maxArea(height: Array[Int]): Int = {
    var i = 0
    var j = height.length - 1
    var max = 0
    while (i < j) {
      val min = scala.math.min(height(i), height(j))
      val area = (j - i) * min
      max = scala.math.max(max, area)
      if (height(i) <= height(j)) i += 1
      else j -= 1
    }
    max
  }
}