(1 to 1000).toStream

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  print(lo+" ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

streamRange(1, 10).take(3).toList

def expr = {
  val x = { print("x"); 1  }
  lazy val y = { print("y"); 2  }
  def z = { print("z"); 3 }
  z + y + x + z + y + x
}
expr

// Infinite streams
def from(n: Int): Stream[Int] = n #:: from(n+1)
val nats = from(0)
val m4s = nats map (_ * 4)
m4s take(400) toList

def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))

primes take 100 toList

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0001

sqrtStream(4).take(4).toList

sqrtStream(4) filter (isGoodEnough(_, 4)) take 10 toList