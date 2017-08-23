package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val delta = Math.pow(b.apply, 2) - a.apply * c.apply
      if (delta <= 0) 0
      else delta
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val root1 = (-1*b.apply + Math.sqrt(delta.apply)) / (2 * a.apply)
      val root2 = (-1*b.apply - Math.sqrt(delta.apply)) / (2 * a.apply)
      Set(root1,root2)
    }
  }
}
