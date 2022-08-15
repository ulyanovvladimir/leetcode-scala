import scala.collection.mutable

object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val hash: mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()

    for (i <- nums.indices) {
      val x: Int = nums(i)
      val y: Int = target - x
      val yPosition = hash.get(y)
      if (yPosition.isDefined) {
        return Array(yPosition.get, i)
      } else {
        hash += (x -> i)
      }
    }
    Array()
  }
}