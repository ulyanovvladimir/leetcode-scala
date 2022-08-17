/*

TASK: n queens
Input: n = 4
Output: [[".Q..","...Q","Q...","..Q."],["..Q.","Q...","...Q",".Q.."]]
Explanation: There exist two distinct solutions to the 4-queens puzzle as shown above

Example 2:
Input: n = 1
Output: [["Q"]]


SOLUTION
List[Int] is a representation for one board where
index = row
value= column
i.e.
List(1,3,0,2)     ->       [".Q..","...Q","Q...","..Q."]

permutations of (0..2) gives you an iterator of possible (0,1,2), (0,2,1),...
by design they have no clashes with horizontals and verticals: only one number per each row, and unique number means unique column

The only thing we need is to check diagonals. If we subtract `column-row` diagonal clash will become vertical clash
(1,2,0) -> (1, 1,-2) -> clash!
*/

object Solution {
  def solveNQueens(n: Int): List[List[String]] = {
    def diagonalCheck(ite: Iterator[Int]) = ite
      .zipWithIndex
      .map { case (pos, index) => pos - index }
      .distinct
      .length == n

    def resultRow(pos: Int): String = (for (i <- 0 until n) yield if (i == pos) 'Q' else '.').mkString

    def result(board: IndexedSeq[Int]) = board.map(resultRow).toList

    (0 until n).permutations
      .filter(board => diagonalCheck(board.iterator) && diagonalCheck(board.reverseIterator))
      .map(board => result(board)).toList
  }
}