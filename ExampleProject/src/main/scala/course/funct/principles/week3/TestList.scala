package week3

/**
  * Created by tryildse on 6/12/17.
  */
object TestList extends App{

  def nth[T](index: Int, list: List[T]) : T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException
    else if (index == 0) list.head
    else nth(index-1, list.tail)


  }

  var list: List[Int] = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, Nil)))

  println(nth(0, list))


}
