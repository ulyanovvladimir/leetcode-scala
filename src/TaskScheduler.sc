object Solution {
  // Math solution
  def leastInterval(tasks: Array[Char], n: Int): Int = {
    val chars = tasks.groupBy(x => x).map(_._2.size).toSeq.sortBy(-_)
    val nmax = chars.takeWhile(_ == chars.head).size
    math.max(tasks.size, (chars.head - 1) * (n + 1) + nmax)
  }
}

Solution.leastInterval(Array('A', 'A', 'A', 'A', 'A', 'A', 'B', 'C', 'D', 'E', 'F', 'G'), 2) == 16
Solution.leastInterval(Array('A', 'A', 'A', 'B', 'B', 'B'), 2) == 8
Solution.leastInterval(Array('A', 'A', 'A', 'B', 'B', 'B'), 0) == 6

/*
Input: tasks = ['A','A','A','B','B','B'], n = 2
Output: 8
Explanation:
A -> B -> idle -> A -> B -> idle -> A -> B
There is at least 2 units of time between any two same tasks.
MATH:
A -> 3
B -> 3
(3-1)*(2+1)+2 = 8

Input: tasks = ['A','A','A','A','A','A','B','C','D','E','F','G'], n = 2
Output: 16
Explanation:
One possible solution is
A -> B -> C -> A -> D -> E -> A -> F -> G -> A -> idle -> idle -> A -> idle -> idle -> A

A -> 6
B -> 1
C -> 1
D -> 1
E -> 1
F -> 1
G -> 1

MATH: (6-1)*(2+1) + 1 = 16
 */