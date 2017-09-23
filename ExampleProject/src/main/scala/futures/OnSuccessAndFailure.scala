package futures

import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.util.Random
import java.lang.Thread._

object OnSuccessAndFailure extends App {
    val f = Future {
        sleep(Random.nextInt(500))
        if (Random.nextInt(500) > 250) throw new Exception("Yikes!") else 42
    }
    f onSuccess {
        case result => println(s"Success: $result")
    }
    f onFailure {
        case t => println(s"Exception: ${t.getMessage}")
    }

    // do the rest of your work
    println("A ..."); sleep(100)
    println("B ..."); sleep(100)
    println("C ..."); sleep(100)
    println("D ..."); sleep(100)
    println("E ..."); sleep(100)
    println("F ..."); sleep(100)
    sleep(2000)
}