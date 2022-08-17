object Solution {
  def lengthOfLongestSubstring(s: String): Int = {
    val hash = new collection.mutable.HashSet[Char].empty

    var ret = 0
    var start = 0
    var end = 0

    while (end < s.length) {
      val next = s(end)

      if(!hash.add(next)) {
        ret = Math.max(ret, end-start)
        while (s(start) != next) {
          hash.remove(s(start))
          start+=1
        }
        start +=1
      }

      end += 1
    }

    Math.max(ret, end-start)
  }
}