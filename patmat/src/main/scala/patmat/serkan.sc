"Hello".groupBy(identity).map{ case (key, value)=>
    key -> value.size
}

val l = List("asdas", "asdasd")
val l4 = List("fddffd", "sdsdfsff")
val l2 = "asdad"

val l3 = l2 :: l