//object session {
  def sqrtIter(guess: Double, x: Double): Double = {
    if(isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    Math.abs(guess * guess - x) < 0.001
  }

  def improve(guess: Double, x: Double): Double = {
    (guess + x / guess) / 2
  }

  def sqrt(x: Double): Double = sqrtIter(1.0, x)

  sqrt(2)
//}

1 + 3