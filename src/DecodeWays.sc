object Solution {
  val decodeSet: Set[String] = (1 to 26).map(_.toString).toSet

  def numDecodings(s: String): Int = {
    val mem = scala.collection.mutable.HashMap.empty[Int, Int]

    def memoizedDecodings(i: Int): Int = {
      if (mem.contains(i)) mem(i)
      else {
        val response =
          if (i == s.length) 1
          else if (i == s.length - 1 && decodeSet.contains(s.slice(i, i+1))) 1
          else if (i == s.length - 1) 0
          else {
            val first = s.slice(i, i + 1)
            val firstTwo = s.slice(i, i + 2)
            if (decodeSet.contains(first) && decodeSet.contains(firstTwo))
              memoizedDecodings(i + 1) + memoizedDecodings(i + 2)
            else if (decodeSet.contains(first)) memoizedDecodings(i + 1)
            else 0
          }
        mem.put(i, response)
        response
      }
    }

    memoizedDecodings(0)
  }
}