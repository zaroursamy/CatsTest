import cats.Eval
import cats.data.IndexedStateT

case class Coin()
case class Bottle()

sealed trait MachineError

object MachineError {

  // bouteille coincée
  case object BottleStuck extends MachineError

  // plus de bouteilles dans la machine
  case object EmptyStock extends MachineError

}

// donne une bouteille et ouvre la porte
object ApiInternalMachine {
  def giveBottle(): Boolean = scala.math.random > 0.001

  def openDoor(): Boolean = true
}

object Machine {

  import MachineError._
  import cats.data.State

  type BottleOrError = Either[MachineError, Bottle]

  def sell(c: Coin): State[Int, BottleOrError] = State({
    case stock: Int if stock > 0 && ApiInternalMachine.giveBottle() => (stock - 1, Right(Bottle()))
    case stock: Int if stock > 0 => (stock - 1, Left(BottleStuck))
    case stock: Int => (stock, Left(EmptyStock))
  })

  def getBottles(bottles: Seq[Bottle]): State[Int, Boolean] = State({ stock: Int =>
    if (stock > 0 && ApiInternalMachine.openDoor()) (bottles.length + stock, true)
    else (stock, false)
  })
}

object Run {

  import Machine._

  def main(args: Array[String]): Unit = {

    val state: IndexedStateT[Eval, Int, Int, List[BottleOrError]] = for {
      maybe1 <- sell(Coin())
      maybe2 <- sell(Coin())
      maybe3 <- sell(Coin())
    } yield List(maybe1, maybe2, maybe3)

    // On démarre la machine avec un stock initial de 2 cannettes
    val finalState: (Int, List[BottleOrError]) = state.run(2).value
    println(s"stock: ${finalState._1}")
    println(s"results: ${finalState._2}")
  }
}
