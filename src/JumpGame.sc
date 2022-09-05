object Solution {
  def canJump(nums: Array[Int]): Boolean = {
    //  means jump length to the index that can reach the final index
    var jumpToKnown = 1

    for (i <- (nums.length - 2) to 0 by -1) {
      if (nums(i) >= jumpToKnown) jumpToKnown = 1
      else jumpToKnown += 1
    }

    jumpToKnown == 1
  }
}

Solution.canJump(Array(2, 3, 1, 1, 4))
!Solution.canJump(Array(3, 2, 1, 0, 4))
Solution.canJump(Array(2, 0))
!Solution.canJump(Array(9997) ++ (9997 to 0 by -1) ++ Array(0))