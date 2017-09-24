package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

//    def balanceHelper(subChars: List[Char], acc: Int): Int = {
//      subChars match {
//        case List() => acc
//        case x :: xs if (x == '(') => balanceHelper(xs, acc + 1)
//        case x :: xs if (x == ')') => balanceHelper(xs, acc - 1)
//        case x :: xs if (x != '(' && x != ')') =>  balanceHelper(xs, acc)
//      }
//
//    }
//
//    balanceHelper(chars.toList, 0) == 0

    var acc = 0
    var i = 0
    while (i < chars.length) {
      if (acc >= 0) {
        if (chars(i) == '(') acc += 1
        else if (chars(i) == ')') acc -= 1
      }
      i += 1
    }
    if (acc == 0) true
    else false
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until)
        (arg1, arg2)
      else {
        val (newArg1, newArg2) = {
          if (chars(idx) == '(') (arg1 + 1, arg2)
          else if (chars(idx) == ')') (arg1, arg2 + 1)
          else (arg1, arg2)
        }

        traverse(idx + 1, until, newArg1, newArg2)
      }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from < threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val ((a1,a2), (b1,b2)) = parallel(reduce(from, mid), reduce(mid, until))
        (a1+b1, a2+b2)
      }
    }

    val (left, right) =  reduce(0, chars.length)
    left == right
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
