import scala.concurrent.{Future, Promise}

case class TaxCut(reduction: Int)

// either give the type as a type parameter to the factory method:
val taxcut = Promise[TaxCut]()

// or give the compiler a hint by specifying the type of your val:
val taxcut2: Promise[TaxCut] = Promise()

val taxcutF: Future[TaxCut] = taxcut.future

taxcut.success(TaxCut(20))