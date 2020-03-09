
class Rational(x: Int, y: Int) {
  require(y != 0, "Denom must be non zero")
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b:Int): Int = if(b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x // g
  def denom = y // g

   def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      that.denom * denom
    )

  override def toString = numer / g + "/" + denom / g


  def unary_- = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

  def < (that: Rational) = if(numer * that.denom  < that.numer * denom) true else false

  def max(that: Rational) = if(this < that) that else this
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
val strange = new Rational(100000, 10000000)

x - y - z

x + x + x
-y
y max z
y < x

// a + b ^? c ?^ d less a ==> b | c
// ((a + b) ^? (c ?^ d)) less ((a ==> b) | c)

//Set
val a = Set()

a.isEmpty
