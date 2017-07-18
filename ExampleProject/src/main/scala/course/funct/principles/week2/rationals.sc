class Rational(x: Int, y:Int) {

  require(y != 0, "Denominator must be nonzero !")

  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int) : Int = if (b == 0) a else gcd(b, a%b)

  val g = gcd(x, y)
  val numer = x
  val denom = y

  def + (that: Rational) : Rational = {
    new Rational(numer*that.denom + that.numer*denom, denom*that.denom)
  }

  def - (that: Rational) : Rational = {
    this + -that
  }

  def unary_- : Rational = {
    new Rational(-1*numer, denom)
  }

  def < (that: Rational) : Boolean = {
    (numer / denom) < (that.numer / that.denom)
  }

  def max(that: Rational) : Rational = {
    if (this < that) that else this
  }

  override def toString: String = numer/g + "/" + denom/g
}

val x = new Rational(1,2)
x.numer
x.denom

def addRational(r1: Rational, r2: Rational) : Rational = {
  new Rational((r1.numer * r2.denom + r2.numer*r1.denom), r1.denom*r2.denom)
}

def toString(r: Rational) =
  r.numer + " / " + r.denom

// toString(addRational(new Rational(1,2),new Rational(3,4)))

val y = new Rational(3,4)

(x + y).toString

val z = new Rational(2,3)

(x- y - z).toString

val n = new Rational(2)

x.max(y)