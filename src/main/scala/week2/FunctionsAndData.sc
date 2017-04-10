class Rational(numer: Int, denom: Int) {
  def add(r: Rational) =
    new Rational(numer * r.denom + r.numer * denom,
      denom * r.denom)

  def neg() = new Rational(-numer, denom)

  def subtract(r: Rational) = add(neg())

  override def toString = numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

z.neg
x.subtract(y)

x.subtract(y).subtract(z)

