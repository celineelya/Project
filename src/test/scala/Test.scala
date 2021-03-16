import org.scalatest.FunSuite
import Ex1.{One, Zero, fromBinaryToInt, fromIntToBinary, Nil}

class Test extends FunSuite {
  test("Assert Inverse Function") {
    for (x<-0 to 10) {
      assert(fromBinaryToInt(fromIntToBinary(x)) == x)
    }
    assert(fromIntToBinary(fromBinaryToInt(One(One(Zero(Nil)))))== One(One(Zero(Nil))))
  }
}