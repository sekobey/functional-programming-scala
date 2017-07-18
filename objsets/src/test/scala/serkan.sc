case class A(i:Int, s:String)

val a = A(1,"abc")

a.copy(i=5)

val l = List(1,2,3,4,5)
l.map(_*2)
l.par.map(_*2)