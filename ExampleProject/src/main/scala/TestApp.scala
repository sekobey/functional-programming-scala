import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success}

/**
  * Created by tryildse on 8/15/17.
  */
object TestApp extends App {

  val firstOccurrence = Future {
    val source = Source.fromFile("/Users/tryildse/Development/Workspace/IdeaProjects/functional-programming-scala/ExampleProject/src/main/resources/ap_footnotes.csv")
    source.toSeq.indexOfSlice("SVF14N12SG")
  }

  firstOccurrence onComplete {
    case Success(value) => println("Index of the term = " + value)
    case Failure(t) => println("An error occured ::: " + t.getMessage)
  }

  println("sadsadassaddsaads")

  val f = Future {
    val x:String = null
    x
  }

  Thread.sleep(10000)

  f onFailure {
    case npe: NullPointerException =>
      println("I'd be amazed if this printed out.")
  }

  println("sadsadassaddsaads")
}
