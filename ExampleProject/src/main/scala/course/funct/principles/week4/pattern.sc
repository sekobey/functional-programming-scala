/**
  * Created by tryildse on 6/14/17.
  */
trait Expr {

  def eval(): Int = {
    this match {
      case Number(n) => n
      case Sum(l, r) => l.eval + r.eval
      case Prod(l, r) => l.eval * r.eval
    }
  }

  def show(): String = {
    this match {
      case Number(n) => n.toString
      case Sum(l, r) => "(" + l.show + " + " + r.show + ")"
      case Prod(l, r) => l.show + " * " + r.show
      case Var(x) => x
    }
  }
}

case class Number(n: Int) extends Expr {}

case class Sum(l: Expr, r: Expr) extends Expr {}

case class Prod(l: Expr, r: Expr) extends Expr {

}

case class Var(name: String) extends Expr {

}

Sum(Number(1), Number(2)).eval

val prod = Prod(Number(2), Sum(Number(1), Number(2)))

Sum(prod, Number(5)).eval