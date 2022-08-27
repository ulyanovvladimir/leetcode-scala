class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def isValidBST(root: TreeNode): Boolean = {
    def isValidBST(current: TreeNode, lowerBound: Long, upperBound: Long): Boolean = {
      current == null || current.value < upperBound &&
        current.value > lowerBound &&
        isValidBST(current.left, lowerBound, current.value) &&
        isValidBST(current.right, current.value, upperBound)
    }

    isValidBST(root, Long.MinValue, Long.MaxValue)
  }
}