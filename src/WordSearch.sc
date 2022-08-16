object Solution {
  def exist(board: Array[Array[Char]], word: String): Boolean = {

    def exist(board: Array[Array[Char]], word: String, y: Int, x: Int, index: Int): Boolean = {
      if (index == word.length) true
      else if (y < 0 || y == board.length || x < 0 || x == board(y).length) false
      else if (board(y)(x) != word(index)) false
      else {
        board(y)(x) = (board(y)(x) ^ 256).asInstanceOf[Char]
        val result: Boolean = exist(board, word, y + 1, x, index + 1) ||
          exist(board, word, y - 1, x, index + 1) ||
          exist(board, word, y, x + 1, index + 1) ||
          exist(board, word, y, x - 1, index + 1)
        board(y)(x) = (board(y)(x) ^ 256).asInstanceOf[Char]
        result
      }
    }

    for (y <- board.indices; x <- board(y).indices) {
      if (exist(board, word, y, x, 0)) return true
    }
    false
  }
}