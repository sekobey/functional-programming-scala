package futures

import java.lang.Thread._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object Cloud {
    def runAlgorithm(i: Int): Future[Int] = Future {
        sleep(Random.nextInt(500))
        val result = i + 10
        println(s"returning result from cloud: $result")
        result
    }
}