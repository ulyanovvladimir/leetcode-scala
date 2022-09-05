object Solution {
  def jump(nums: Array[Int]): Int = {
    if (nums.length == 1) return 0
    var jumps = 0
    var currentFurthest = 0 // check all points and keep track of furthest point we can jump to
    var currentEnd = 0 //next furthest point we can jump to, when i reaches currentEnd, we again update currentEnd= currentfurthest
    for (i <- 0 until nums.length - 1) {
      currentFurthest = currentFurthest.max(nums(i) + i) // keep the point which can reach to furthest
      //When i reaches to end/last point from prev jump
      if (i == currentEnd) {
        jumps += 1
        currentEnd = currentFurthest
      }
    }
    jumps
  }
}

Solution.jump(Array(1)) == 0
Solution.jump(Array(2, 3, 1, 1, 4)) == 2
Solution.jump(Array(2, 3, 0, 1, 4)) == 2
Solution.jump(Array(4, 1, 1)) == 1