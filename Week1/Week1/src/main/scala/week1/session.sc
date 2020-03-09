object session {

  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double): Boolean = {
      Math.abs(guess * guess - x) / x < 0.001
    }

    def improve(guess: Double): Double = {
      (guess + x / guess) / 2
    }

    sqrtIter(1.0)
  }
}

session.sqrt(2)
session.sqrt(1e-9)
session.sqrt(1e60)

object tailRecursion {
  def factorial(x: Int, y: Int = 1): Int = {
    if(x == 0) y else factorial(x - 1, y * x)
  }
}

tailRecursion.factorial(4)
