object Solution {
  def rob(nums: Array[Int]): Int =  {
    if (nums.length == 0) return 0
    var prev1 = 0
    var prev2 = 0

    for (num <- nums) {
      val tmp = prev1
      prev1 = Math.max(prev2 + num, prev1)
      prev2 = tmp
    }
    prev1
  }
}

// Scala FP solution
def rob(nums: Array[Int]): Int = nums.foldLeft((0, 0)) {
  case ((prev, max), num) => (max, max max (prev + num))
}._2


Solution.rob(Array(2, 7, 9, 3, 1)) == 12
Solution.rob(Array(183, 219, 57, 193, 94, 233, 202, 154, 65, 240, 97, 234, 100, 249, 186, 66, 90, 238, 168, 128, 177, 235, 50, 81, 185, 165, 217, 207, 88, 80, 112, 78, 135, 62, 228, 247, 211)) == 3365

/*
(10,7,3,9,1)
(0,0)10 => (0, 10)
(0,10)7 => (10, 10 max 0+7)
(10,10)3 => (10, 10 max 10+3)
(10, 13)9 => (13, 13 max 10+9)
(13, 19)1 => (19, 19 max 13+1)
(19,19)

 */

/*
(2,7,9,3,1)
(0,0)2 => (0, 0 max 2)
(0,2)7 => (2, 2 max 0+7)
(2,7)9 => (7, 7 max 2+9)
(7,11)3 => (11, 11 max 7+3)
(11, 11)1 => (11, 11 max 12)
(11,12)
 */

