package course.funct.design.week4

import scala.util.DynamicVariable

/**
  * Created by tryildse on 7/28/17.
  */
class Signal[T](expr: => T) {

  import Signal._
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()

  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if (myValue != newValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }
}

object NoSignal extends Signal[Nothing](???) {
  override protected def computeValue(): Unit = ()
}

object Signal {

  // not thread-safe, because its global sate variable, it may be acessed from many threads
  //  val caller = new StackableVariable[Signal[_]](NoSignal)

  /*
  This usage provides thread-safety for multi-threaded environments. It brings thread-local state instead of global state.
  Thread-local state still comes with a number of disadvantages:
  ▶ Its imperative nature often produces hidden dependencies which are
  hard to manage.
  ▶ Its implementation on the JDK involves a global hash table lookup,
  which can be a performance problem.
  ▶ It does not play well in situations where threads are multiplexed
  between several tasks.

  A cleaner solution involves implicit parameters.
  ▶ Instead of maintaining a thread-local variable, pass its current value
  into a signal expression as an implicit parameter.
  ▶ This is purely functional.
  ▶ In current Scala it requires more boilerplate than the thread-local
  solution.
  ▶ Future versions of Scala might solve that problem.
  */
  val caller = new DynamicVariable[Signal[_]](NoSignal)

  def apply[T](expr: => T) = new Signal(expr)
}
