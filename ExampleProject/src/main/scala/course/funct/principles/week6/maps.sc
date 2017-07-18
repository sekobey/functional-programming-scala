val capitalOfCountry = Map("Turkey" -> "Ankara", "Netherlands" -> "Amsterdam")

capitalOfCountry("Turkey")

capitalOfCountry.get("Switzerland")

def showCapitalOfCountry(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapitalOfCountry("Turkey")
showCapitalOfCountry("Switzerland")

val fruits = List("apple", "pear", "orange", "pineapple")

fruits.sortWith(_.length < _.length)
fruits.sorted

fruits.groupBy(_.head)



//====================================================
//====================================================
//====================================================

class Poly(val terms0: Map[Int, Double]) {

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  // solution with concatenation
  /*
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))

  }
  */

  // solution with foldleft
  def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString: String = {
    (for ((exp, coeff) <- terms.toList.reverse.sorted)
      yield coeff+"x^"+exp) mkString "+"

  }
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

p1 + p2

p1.terms(7)