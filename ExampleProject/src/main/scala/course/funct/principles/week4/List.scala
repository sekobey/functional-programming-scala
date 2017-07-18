package week4

import week3.{List, Cons, Nil}

/**
  * Created by tryildse on 6/13/17.
  */
object List {

  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, Nil))

  def apply[T]() = Nil

}
