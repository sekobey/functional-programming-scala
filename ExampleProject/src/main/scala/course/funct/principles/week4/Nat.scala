package week4.nat

/**
  * Created by tryildse on 6/13/17.
  */
abstract class Nat {

  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat

}

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new Exception

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = throw new Exception
}

class Succ(n: Nat) extends Nat {

  def isZero: Boolean = false

  def predecessor: Nat = n

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = new Succ(n + that)

  def -(that: Nat): Nat = if (that.isZero) this else new Succ(n - that.predecessor)
}