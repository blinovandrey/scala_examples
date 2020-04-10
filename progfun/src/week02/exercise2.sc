object exercise2 {
  import math.abs

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double): Boolean = {
    abs((x - y) / x) / x < tolerance
  }

  def fixedPoints(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      println("guess = " + guess)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoints(x => 1 + x/2)(1)

  def averageDump(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) = fixedPoints(averageDump(y => x / y))(1.0)

  sqrt(9)
}