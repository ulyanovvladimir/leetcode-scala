object Solution {
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {

    def validSequence(chars: Seq[Char]): Boolean = {
      val known: scala.collection.mutable.Set[Char] = new scala.collection.mutable.HashSet[Char]().empty
      for (c <- chars) {
        if (c == '.') {}
        else if (known.contains(c)) return false
        else known += c
      }
      true
    }

    def rowValid(board: Array[Array[Char]], row: Int): Boolean = {
      validSequence(board(row))
    }

    def columnValid(board: Array[Array[Char]], column: Int): Boolean = {
      validSequence((0 until 9).map(row => board(row)(column)))
    }


    def blockValid(board: Array[Array[Char]], row: Int, column: Int): Boolean = {
      val sequence = for (i <- 0 until 3; j <- 0 until 3) yield board(row + i)(column + j)
      validSequence(sequence)
    }

    for (x <- 0 until 9) if (!rowValid(board, x)) return false
    for (y <- 0 until 9) if (!columnValid(board, y)) return false
    for (x <- 0 until 9 by 3; y <- 0 until 9 by 3) if (!blockValid(board, x, y)) return false

    true
  }
}