import scala.collection.mutable

object Solution {
  // Math solution
  def taskSchedulerII(tasks: Array[Int], space: Int): Long = {
    var days = 0L
    val taskLastTime = new mutable.HashMap[Int, Long]()
    for (i <- tasks.indices) {
      days = taskLastTime.get(tasks(i))
        .map(lastTime => math.max(days, space + lastTime) + 1)
        .getOrElse(days + 1)
      taskLastTime.put(tasks(i), days)
    }
    days
  }
}
// 3+1 - (1 - 1)

Solution.taskSchedulerII(Array(1, 2, 3), 3) == 3
Solution.taskSchedulerII(Array(1, 1), 3) == 5
// 1 idle idle idle 1
Solution.taskSchedulerII(Array(1, 2, 1), 3) == 5
Solution.taskSchedulerII(Array(1, 2, 1, 2, 3, 1), 3) == 9
// 1 2 idle idle 1 2 3 idle 1
Solution.taskSchedulerII(Array(5, 8, 8, 5), 2) == 6
// 5 8 idle idle 8 5