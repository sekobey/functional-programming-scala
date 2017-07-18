import week3.{Cons, Empty, IntSet, Nil, NonEmpty}
import week4.nat.{Succ, Zero}
import week4.pattern._

def m(i:Int)= i*3

val xs= List(1,2,3,4)
xs.sum
xs.map(_*2)
m(5)
xs.map( i=> m(i))
xs.filter(i => i>3)

class A(i:Int){
  def +(y:Int) = i+y
}

new A(10).+(2)
new A(10) +(2)
new A(10) + 2

def radius = 10
var radius2 = 10
println(radius)
print(radius2)

def square(x:Double) = x * x
square(2)
square(4+5)

def sumOfSquares(x:Double, y:Double) = square(x) + square(y)
sumOfSquares(1,2)

def loop:Boolean = loop

def y = loop

def and(x:Boolean,y: =>Boolean) = if (x) y else false

and(false, loop)

(x : Int) => x * x * x

def mapReduce(f: Int => Int, combine:(Int, Int) => Int, zero:Int)(a: Int, b: Int) : Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1,b))
}

/*
def product(f : Int => Int)(a: Int, b:Int) : Int = {
  def prod(a: Int, acc: Int) : Int = {
    if (a > b) acc
    else prod(a+1, f(a)*acc)
  }

  prod(a, 1)
}
*/

def product(f : Int => Int)(a: Int, b:Int) : Int = {
  mapReduce(f, (x,y) => x*y, 1)(a,b)
}

def sum(f: Int => Int)(a: Int, b:Int) : Int = {
  mapReduce(f, (x,y) => x+y, 0)(a,b)
}

product(x => x * x)(3, 4)
sum(x => x)(3,4)

def fact(x: Int): Int = {
  product(x => x)(1, x)
}

fact(5)

type Set = Int => Boolean

def contains(s: Set, elem: Int): Boolean = s(elem)

contains(x => x < 0, -11)

type MyType = String => Int

def myVar(x:String) : MyType = x => Integer.parseInt(x) * 2

println(myVar("3"))

def nth[T](index: Int, list: week3.List[T]) : T = {
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (index == 0) list.head
  else nth(index-1, list.tail)


}

var list: week3.List[Int] = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, Nil)))

println(nth(0, list))

val nat = Zero + new Succ(Zero)

nat.isZero
nat.predecessor


List(1,2)



/*
val a: Array[NonEmpty] = Array(new  NonEmpty(1, new Empty, new Empty))
val b: Array[IntSet] = a
b(0) = new Empty
val s: NonEmpty = a(0)
*/
//Number(5).eval
