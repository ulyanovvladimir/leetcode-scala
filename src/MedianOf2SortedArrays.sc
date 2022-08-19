object Solution {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val arr: Array[Int] = new Array(nums1.length + nums2.length)
    var i: Int = 0
    var j: Int = 0

    while (i < nums1.length || j < nums2.length) {
      if (i == nums1.length || (j < nums2.length && nums1(i) > nums2(j))) {
        arr(i + j) = nums2(j)
        j += 1
      } else {
        arr(i + j) = nums1(i)
        i += 1
      }
    }

    if (arr.length % 2 == 1) arr(arr.length / 2) else (arr(arr.length / 2) + arr(arr.length / 2 - 1)) * 0.5
  }
}