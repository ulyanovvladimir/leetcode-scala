object Solution {
  def memoize[K, V](f: K => V): K => V = {
    val cache = scala.collection.mutable.Map.empty[K, V]
    k => cache.getOrElseUpdate(k, f(k))
  }

  val fibonachi: Int => Int = memoize {
    case 0 => 0
    case 1 => 1
    case n => fibonachi(n - 2) + fibonachi(n - 1)
  }

  def climbStairs: Int => Int = memoize {
    case 1 => 1
    case n => climbStairs(n - 1) + fibonachi(n - 1)
  }
}

Solution.climbStairs(45)
Solution.climbStairs(1) == 1
Solution.climbStairs(2) == 2
Solution.climbStairs(3)== 3
Solution.climbStairs(4)== 5
Solution.climbStairs(5) == 8
Solution.climbStairs(6) == 13

//FORMULA climbstairs(n-1)+ fib(n-1)

/*
n =1 => 1
n=2 => 2   = 1+fib(2-1) = 2
n = 3 => 3    = +fib(3-1)
  1+1+1
  1+2     *
  2+1
n = 4 => 5
  1+1+1+1
  1+1+2   *
  1+2+1
  2+1+1
  2+2     *

n = 5 => 8
  1+1+1+1+1
  1+1+1+2 *
  1+1+2+1
  1+2+1+1
  1+2+2   *
  2+1+1+1
  2+1+2   *
  2+2+1

n = 6 => 13
  1+1+1+1+1+1
  1+1+1+1+2   *
  1+1+1+2+1
  1+1+2+1+1
  1+1+2+2     *
  1+2+1+1+1
  1+2+1+2     *
  1+2+2+1
  2+1+1+1+1
  2+1+1+2     *
  2+1+2+1
  2+2+1+1
  2+2+2       *
 */