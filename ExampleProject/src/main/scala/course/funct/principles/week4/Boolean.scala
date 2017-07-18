package week4

/**
  * Created by tryildse on 6/13/17.
  */
abstract class Boolean {

  def ifThenElse[T](t: => T, e: => T) : Boolean

  def && (x: => Boolean) : Boolean = ifThenElse(x, false)

  def || (x: => Boolean) : Boolean = ifThenElse(true, x)

  def unary_! : Boolean = ifThenElse(false, true)

  def == (x: Boolean) : Boolean = ifThenElse(x, !x)

  def != (x: Boolean) : Boolean = ifThenElse(!x, x)

  def < (x: => Boolean) : Boolean = ifThenElse(false, x)

}

