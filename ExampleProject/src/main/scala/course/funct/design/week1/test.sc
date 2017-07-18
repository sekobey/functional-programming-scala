val f: String => String = {case "ping" => "pong"}

f("ping")

// f("pong")

val g: PartialFunction[String, String] = {case "ping" => "pong"}

g("ping")

g.isDefinedAt("ping")

g.isDefinedAt("pong")

case class Book(title: String, authors: List[String]) {}

val books: List[Book] = List(
  Book(title = "Structure and Interpretation of Computer Programs",
authors = List("Abelson, Harald", "Sussman, Gerald J.")),
Book(title = "Introduction to Functional Programming",
authors = List("Bird, Richard", "Wadler, Phil")),
Book(title = "Effective Java",
authors = List("Bloch, Joshua")),
Book(title = "Java Puzzlers",
authors = List("Bloch, Joshua", "Gafter, Neal")),
Book(title = "Programming in Scala",
authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

// Find authors which have at least 2 books ???
(
for {
  b1 <- books
  b2 <- books
  if (b1 != b2)
  a1 <- b1.authors
  a2 <- b2.authors
  if (a1 == a2)
} yield a1
  ).distinct

// example for converting a for expression to higher order functions
for (b <- books; a <- b.authors if a startsWith "Bird")
  yield b.title

books.flatMap (b =>
  b.authors.withFilter(a => a.startsWith("Bird")) map (_ => b.title)
)