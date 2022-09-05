object Solution {
  def maxProfit(prices: Array[Int]): Int =
    prices.foldLeft((Int.MaxValue, 0)) {
      case ((minPrice, maxProfit), price) => (minPrice min price, maxProfit max (price - minPrice))
    }._2
}

// optimised solution
def maxProfit(prices: Array[Int]): Int = {
  if (prices.isEmpty) return 0
  var minCosts = prices(0)
  var profit = 0
  for (i <- prices.indices) {
    minCosts = Math.min(minCosts, prices(i))
    profit = Math.max(profit, prices(i) - minCosts)
  }
  profit
}


Solution.maxProfit(Array(7,1,5,3,6,4)) == 5
Solution.maxProfit(Array(7,6,4,3,1)) == 0