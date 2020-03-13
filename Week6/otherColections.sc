def isPrime(n: Int): Boolean = (2 until n) forall (ele => n % ele != 0)

isPrime(7)
isPrime(14)

val n = 7
(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair => isPrime((pair._1 + pair._2)))

// for expression

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
  (for ((x, y) <- xs zip ys) yield x * y).sum
}

scalarProduct(List(1.0, 2.0, 3.0), List(3.0, 2.0, 6.0, 5.0))


 def queens(n: Int): Set[List[Int]] = {

   def isSafe(col: Int, queens: List[Int]): Boolean = {
     val row = queens.length
     val queensWithRow = (row - 1 to 0 by -1) zip queens
     queensWithRow forall {
       case (r, c) => col != c && math.abs(col - c) != row - r

     }
   }

   def placeQueens(k: Int): Set[List[Int]] = {
     if (k == 0) Set(List())
     else
       for {
         queens <- placeQueens(k - 1)
         col <- 0 until n
         if isSafe(col, queens)
       } yield col :: queens
   }
   placeQueens(n)
 }



queens(4)