val list = 1 :: 2 :: 3 :: 4 :: Nil

val list2 = 5 :: 6 :: 7 :: 8 :: Nil

list last

list take 2

list drop 2

list2 ++ list

list splitAt(1)

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error
  case List(x) => Nil
  case y :: ys => y :: init(ys)
}

init(list)

def msort[T](list: List[T])(implicit ord:Ordering[T]): List[T] = {

  def merge(xs: List[T], ys: List[T]): List[T] = {
    (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) => {
        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
      }
    }
  }

  if (list.isEmpty || list.length == 1)
    list
  else {
    val n = list.size / 2

    val (fHalf, sHalf) = list splitAt n

    merge(msort(fHalf), msort(sHalf))
  }
}

msort(List(4, 2, 9, 1, 3, 5))
msort(List("apple","orange","mellon"))

//====================================================
//====================================================
//====================================================

List(1,4,5) map (x => x*x)

List("Serkan","Naber") map (x => x.reverse)

val xs = List(2, -4, 5, 1, 6, 7)

xs.filter(x => x > 5)
xs.filterNot( x => x > 5)
xs.partition(x => x > 0)
xs.takeWhile( x => x > 0)
xs.dropWhile( x => x > 0)
xs.span( x => x > 0)
xs map (x => x*2) reduce ((x,y) => x + y)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) =  xs1.span(y => y == x)
    (x :: first) :: pack(rest)
  }
}

val exStrings = List("a", "x", "a", "a", "b", "b", "c", "c", "a")
pack(exStrings)

def encode[T](xs: List[T]): List[(T,Int)] = {
  pack(xs) map (xs => (xs.head, xs.length))
}

encode(exStrings)

exStrings map (x => x.concat("1")) reduceLeft ((x,y) => x.concat(y))

//====================================================
//====================================================
//====================================================

val s = "Hello World !!!"
s filter (c => c.isUpper)
s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List (1,2,3) zip s
pairs unzip

s.flatMap(c => List('.', c))

val xs1 = List(1,5,2,8,3)

xs1.sum
xs1.product
xs1.max
xs1.min

((1 to 10) map (x => (1 to 5) map (y => (x,y)))).flatten
// equal
(1 to 10) flatMap (x => (1 to 5) map (y => (x,y)))

def scalarProduct(list1: Vector[Double], list2: Vector[Double]): Double =
  (list1 zip list2).map(xy => xy._1 * xy._2).sum

def scalarProduct2(list1: Vector[Double], list2: Vector[Double]): Double =
  (list1 zip list2).map{case (x,y) => x * y}.sum

def isPrime(n: Int) = (2 until n) forall (x => n % x != 0)

isPrime(12)

val n = 7

// (1 until n) map (i => (1 until i) map (j => (j,i))) flatten filter (pair => isPrime(pair._1 + pair._2 ))

//(1 until n) flatMap (i => (1 until i) map (j => (j,i))).filter(pair => isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i +j) && i + j < n
} yield (i,j)

def scalarProduct3(list1: List[Double], list2: List[Double]): List[Double] =
  for ( (x,y) <- list1 zip list2 ) yield x * y

scalarProduct3(List(1,2,3), List(-1,-2,-3))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

mapFun[Int,Int](List(1,2,3), (x => x * 2))

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_,y) => 1 + y )

lengthFun(List(1,2,3,45))