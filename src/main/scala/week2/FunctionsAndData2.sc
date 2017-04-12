class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def add(r: Rational) =
    new Rational(numer * r.denom + r.numer * denom,
      denom * r.denom)

  def neg() = new Rational(-numer, denom)
  def subtract(r: Rational) = add(r.neg())

  override def toString = (numer/gcd(numer, denom)) + "/" + (denom/gcd(numer, denom))
}

val x = new Rational(1, 1)
val y = new Rational(1, 2)
val z = new Rational(3, 2)

x.subtract(y)

new Rational(10, 70)