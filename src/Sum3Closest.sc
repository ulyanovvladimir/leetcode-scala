import scala.annotation.tailrec
import scala.util.Sorting

object Solution {
  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    Sorting.quickSort(nums)

    @tailrec
    def helper(n: Int, left: Int, right: Int, diff: Int): Int = {
      if (left < right) {
        val sum = n + nums(left) + nums(right)
        if (Math.abs(sum - target) < Math.abs(diff))
          helper(n, left, right, target - sum)
        else if (sum < target)
          helper(n, left + 1, right, diff)
        else
          helper(n, left, right - 1, diff)
      } else
        diff
    }

    @tailrec
    def iter(i: Int, diff: Int): Int = {
      if (i < nums.length) {
        iter(i + 1, helper(nums(i), i + 1, nums.length - 1, diff))
      } else diff
    }

    target - iter(0, Int.MaxValue)
  }

}



/*
scala FP solution with combinations
 */

def threeSumClosest(nums: Array[Int], target: Int): Int = {

  var closest = nums(0) + nums(1) + nums(2)
  nums.combinations(3).
    foreach(arr => {
      val sum = arr(0) + arr(1) + arr(2)
      if (math.abs(sum - target) < math.abs(closest - target)) closest = sum
    })
  closest
}

Solution.threeSumClosest(Array(0,0,0),1) == 0
Solution.threeSumClosest(Array(-1, 2, 1, -4), 1) == 2
Solution.threeSumClosest(Array(11,-614,236,-443,-263,-492,-194,-806,798,-995,-346,557,253,-36,279,-873,-968,-191,-907,-370,104,976,177,927,-143,137,-843,88,-867,-243,-786,-421,19,305,611,-559,-24,944,499,531,186,-205,-434,-395,-417,-933,-710,-424,-515,351,-797,854,-563,-912,-340,-323,-550,-229,436,-785,999,-923,-350,146,-532,-574,-962,-184,198,287,59,704,-129,672,-959,861,-32,-318,451,813,-621,188,-611,808,408,-833,-108,46,-758,-770,-242,-827,-644,-74,-149,224,-951,87,138,565,-215,155,-45,68,315,144,-483,-150,-718,-595,-509,-68,-2,348,-581,993,262,803,153,409,-326,858,-588,-522,-151,779,-287,-565,8,-777,256,-393,916,965,-730,71,-918,619,208,-491,-717,501,465,578,382,721,-349,-875,-638,405,-694,113,406,860,-297,-13,341,426,915,556,536,330,-526,116,22,-896,-451,-88,322,307,248,663,932,383,-180,-75,-697,47,403,302,197,974,638,497,816,-682,-465,-520,175,-905,75,419,930,447,571,-911,-981,-660,720,-917,-273,856,-106,-281,-939,-161,777,-976,-510,975,566), -9836) == -2952