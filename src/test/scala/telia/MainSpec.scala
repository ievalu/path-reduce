package telia

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class MainSpec extends FunSuite {

  val north            = "NORTH"
  val south            = "SOUTH"
  val east             = "EAST"
  val west             = "WEST"
  val invalidDirection = "INVALID DIRECTION"

  val invalidDirectionError = "Provided list contains unsupported value for path"

  test("test1") {
    Main.pathReduce(List(north, south, south, east, west, north, west)) shouldBe Right(List(west))
  }

  test("test2") {
    Main.pathReduce(List(north, south, south, east, west, north)) shouldBe Right(List())
  }

  test("test3") {
    Main.pathReduce(List(north, west, south, east)) shouldBe Right(List(north, west, south, east))
  }

  test("test4") {
    Main.pathReduce(
      List(south, north, north, east, east, west, north, south, south, west, east, south, east, north, west, east)
    ) shouldBe Right(List(north, east, south, south, east, north))
  }

  test("test5") {
    Main.pathReduce(List()) shouldBe Right(List())
  }

  test("test6") {
    Main.pathReduce(List(south, north, invalidDirection)) shouldBe Left(invalidDirectionError)
  }

  test("test7") {
    Main.pathReduce(List(invalidDirection, south, north)) shouldBe Left(invalidDirectionError)
  }

  test("test8") {
    Main.pathReduce(List(south, invalidDirection, north)) shouldBe Left(invalidDirectionError)
  }

}
