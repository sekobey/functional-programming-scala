package course.funct.design.week4

/**
  * Created by tryildse on 7/28/17.
  */
class Var[T](expr: => T) extends Signal[T](expr){
  override def update(expr: => T): Unit = super.update(expr)
}

object Var{
  def apply[T](expr: => T) = new Var(expr)
}