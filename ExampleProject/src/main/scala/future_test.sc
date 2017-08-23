import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.io.Source
import scala.util.{Failure, Success}
import java.lang.Thread._
import scala.concurrent.ExecutionContext.Implicits.global

val firstOccurrence = Future {
  val source = Source.fromFile("/Users/tryildse/Development/Workspace/IdeaProjects/functional-programming-scala/ExampleProject/src/main/resources/ap_footnotes.csv")
  source.toSeq.indexOfSlice("VPCCB48FN")
}

firstOccurrence onComplete {
  case Success(value) => println("Index of the term = " + value)
  case Failure(t) => println("An error occured ::: " + t.getMessage)
}


val f = Future {
  null
}


f onFailure {
  case npe: NullPointerException =>
    println("I'd be amazed if this printed out.")
}

@volatile var totalA = 0

val text = Future {
  "na" * 16 + "BATMAN!!!"
}

text onSuccess {
  case txt => totalA += txt.count(_ == 'a')
    println(totalA + " in == 'a'")
}

text onSuccess {
  case txt => totalA += txt.count(_ == 'A')
    println(totalA + " in == 'A'")
}

