package week03

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must not equals zero")

  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  def this(x: Int) = this(x, 1)

  val numer = x
  val denum = y

  def +(that: Rational) = {
    new Rational(
      numer * that.denum + denum * that.numer,
      denum * that.denum)
  }

  def <(that: Rational) = numer * that.denum < that.numer * denum

  def max(that: Rational) = if (this < that) that else this

  def unary_- : Rational = new Rational(-numer, denum)

  def - (that: Rational) = this + -that

  override def toString = numer / gcd(x, y) + "/" + denum / gcd(x, y)
}
