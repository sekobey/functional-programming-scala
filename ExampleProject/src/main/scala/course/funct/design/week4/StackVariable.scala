package course.funct.design.week4

/**
  * Created by tryildse on 7/28/17.
  */
class StackableVariable[T](init: T) {

  private var values: List[T] = List(init)

  def value: T = values.head

  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op finally values = values.tail
  }
}
