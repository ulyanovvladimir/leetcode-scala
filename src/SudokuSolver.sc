object Solution {
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    /* three extra tables to record whether coordinate is occupied */
    val columns = Array.ofDim[Boolean](board.length, board.length)
    val rows = Array.ofDim[Boolean](board.length, board.length)
    /**
     * block index:
     * 1 2 3
     * 4 5 6
     * 7 8 9
     * convert (rowIndex, columnIndex) to blockIndex:  ( rowIndex / 3 ) * 3 + ( columnIndex / 3)
     */
    val blocks = Array.ofDim[Boolean](board.length, board.length)

    /* DFS worker*/
    def _solveSudoku(board: Array[Array[Char]], coordQueue: List[(Int, Int)], answer: Array[Array[Char]], checkValid: ((Int, Int), Char) => Boolean): Boolean = {
      coordQueue match {
        /* DFS not complete case : coordinate queue non empty */
        case coord :: remainingQueue =>
          ('1' to '9').filter(checkValid(coord, _)).exists { char =>
            val (rowIdx, colIdx) = coord

            /* set board with char value by coordinate */
            board(rowIdx)(colIdx) = char
            rows(rowIdx)(char.asDigit - 1) = true
            columns(colIdx)(char.asDigit - 1) = true
            blocks((rowIdx / 3) * 3 + (colIdx / 3))(char.asDigit - 1) = true

            val ret = _solveSudoku(board, remainingQueue, answer, checkValid)

            /* recover to status before calling  _solveSudoku
            *  reset board table, rows table, columns table and blocks table
            * */
            board(rowIdx)(colIdx) = '.'
            rows(rowIdx)(char.asDigit - 1) = false
            columns(colIdx)(char.asDigit - 1) = false
            blocks((rowIdx / 3) * 3 + (colIdx / 3))(char.asDigit - 1) = false
            ret
          }

        /* coordinate queue is empty, copy board to answer */
        case Nil =>
          board.zipWithIndex.foreach {
            case (arr, index) => answer(index) = arr.clone
          }
          true
      }
    }

    /* generate all empty coordinates */
    val coords = for (i <- board.indices.toList; j <- board.indices; if board(i)(j) == '.') yield (i, j)

    /* initial rows table, columns table, blocks table */
    for (i <- board.indices; j <- board.indices; if board(i)(j) != '.') {
      val charIdx = board(i)(j).asDigit - 1 // array index range from 0 to 8
      rows(i)(charIdx) = true
      columns(j)(charIdx) = true
      blocks((i / 3) * 3 + (j / 3))(charIdx) = true
    }
    val isValidFunc = isValid(_, _, rows, columns, blocks) // curry function

    _solveSudoku(board.clone(), coords, board, isValidFunc)
  }

  /* check input char value is valid at the coordinate */
  def isValid(coord: (Int, Int), value: Char, rows: Array[Array[Boolean]], columns: Array[Array[Boolean]], blocks: Array[Array[Boolean]]): Boolean = {
    val (row, col) = coord
    val charIdx = value.asDigit - 1
    val blockIdx = (row / 3) * 3 + (col / 3)
    !rows(row)(charIdx) && !columns(col)(charIdx) && !blocks(blockIdx)(charIdx)
  }
}