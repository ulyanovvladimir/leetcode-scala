object Solution {
  def numIslands(grid: Array[Array[Char]]): Int = {
    def destroyIsland(x: Int, y: Int): Unit = {
      if (x >= 0 && x < grid(0).length && y >= 0 && y < grid.length && grid(y)(x) == '1') {
        grid(y)(x) = '0'
        destroyIsland(x - 1, y)
        destroyIsland(x + 1, y)
        destroyIsland(x, y - 1)
        destroyIsland(x, y + 1)
      }
    }

    var islands = 0
    for (y <- grid.indices; x <- grid(y).indices if grid(y)(x) == '1') {
      islands += 1
      destroyIsland(x, y)
    }

    islands
  }
}

Solution.numIslands(Array(
  Array('1', '1', '1', '1', '0'),
  Array('1', '1', '0', '1', '0'),
  Array('1', '1', '0', '0', '0'),
  Array('0', '0', '0', '0', '0')
)) == 1

Solution.numIslands(Array(
  Array('1', '1', '0', '0', '0'),
  Array('1', '1', '0', '0', '0'),
  Array('0', '0', '1', '0', '0'),
  Array('0', '0', '0', '1', '1')
)) == 3
