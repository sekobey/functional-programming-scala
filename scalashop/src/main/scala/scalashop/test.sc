val l1 = List(1, 2, 3)
val l2 = List(4, 6, 7)

l1 zip l2

l1.tail

val width = 3
val numSteps = 4

val range = 0 to width by numSteps
val range1 = 0 to (width - numSteps) by numSteps
val range2 = numSteps to width by numSteps

//val splits = range1 zip range2
//if (width % 32 != 0) splits :+ (range2.last, width)


def splits1(range: List[Int], max: Int): List[(Int, Int)] = {

  def splits1Helper(subRange: List[Int]): List[(Int, Int)] = subRange match {
    case x :: List() => List()
    case x :: xs => (x, xs.head) :: splits1Helper(xs)
  }

  val splits = splits1Helper(range)
  if (splits.last._2 < max) splits :+ (splits.last._2, max)
  else splits
}


def splits(max: Int, numOfSteps: Int) = max match {
  case x if x <= numOfSteps => Vector((0, max))
  case _ =>
    val range1 = 0 to (max - numOfSteps) by numOfSteps
    val range2 = numOfSteps to max by numOfSteps

    val splits = range1 zip range2
    if (splits.last._2 < max) splits :+ (splits.last._2, max)
    else splits
}

splits(3, 4)
splits(1080, 32)
