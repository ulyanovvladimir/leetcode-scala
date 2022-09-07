object Solution {
  def findMinArrowShots(points: Array[Array[Int]]): Int = {
    points.sortBy(_ (0))
      .foldLeft((Int.MinValue, 0)) { case ((prevEnd, count), point) =>
        if (point(0) > prevEnd || count == 0) (point(1), count + 1) else (prevEnd min point(1), count)
      }._2
  }
}

Solution.findMinArrowShots(Array(Array(10, 16), Array(2, 8), Array(2, 6), Array(1, 6), Array(7, 12))) == 2
Solution.findMinArrowShots(Array(Array(2, 5))) == 1
Solution.findMinArrowShots(Array(Array(9, 12), Array(1, 10), Array(4, 11), Array(8, 12), Array(3, 9), Array(6, 9), Array(6, 7))) == 2

/*
 (1,6) (2,8), (7,12), (10,16)
 (1,6) (2,8), (3,5), (7,12), (10,16) => 6, 12
 (2,6), (3,5), (7,12), (10,16)   | 0
 (3,5), (7,12), (10,16) ; 0
 (7,12), (10,16) ; 1
 (10,12) ; 1
  2
*/

