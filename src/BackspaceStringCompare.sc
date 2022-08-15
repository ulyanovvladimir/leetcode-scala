import scala.collection.mutable

object Solution {
  def backspaceCompare(s: String, t: String): Boolean = {
    def build(string: String): String = {
      val buffer = new scala.collection.mutable.ArrayBuffer[Char]()
      for (c <- string) {
        c match {
          case '#' => if (buffer.nonEmpty) buffer.remove(buffer.length - 1)
          case _ => buffer += c
        }
      }
      buffer.mkString
    }

    build(s) == build(t)
  }
}

println(Solution.backspaceCompare("ab#c", "ad#c"))
println(Solution.backspaceCompare("ab##", "c#d#"))
println(Solution.backspaceCompare("a##c", "#a#c"))
