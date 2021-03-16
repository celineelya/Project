import scala.annotation.tailrec

object Ex1 {
  abstract sealed class BinaryNumber {
    def reverseBinaryNumber(): BinaryNumber = {
      @tailrec
      def loop(x: BinaryNumber, y: BinaryNumber): BinaryNumber = x match {
        case Nil => y
        case Zero(xs) => loop(xs, Zero(y))
        case One(xs) => loop(xs, One(y))
      }
      loop(this, Nil)
    }

    override def toString = this match {
      case Zero(x) => "0"+x.toString
      case One(x) => "1"+x.toString
      case Nil => ""
    }
  }
  case class Zero(y: BinaryNumber) extends BinaryNumber
  case class One(y: BinaryNumber) extends BinaryNumber
  case object Nil extends BinaryNumber


  def fromBinaryToInt(x: BinaryNumber): Int = {
    @tailrec
    def loop(x: BinaryNumber, acc: Int, count: Int): Int = {
      x match {
        case Nil => acc
        case Zero(xs) =>  loop(xs, acc, count+1)
        case One(xs) => loop(xs, 1*scala.math.pow(2,count).toInt + acc, count+1)
      }
    }
    loop(x.reverseBinaryNumber(), 0, 0)
  }

  def fromIntToBinary(x: Int) : BinaryNumber = {
    @tailrec
    def loop(x: Int, y: BinaryNumber) : BinaryNumber = {
      if (x != 0) {
        x%2 match {
          case 0 => loop(x/2, Zero(y))
          case 1 => loop(x/2, One(y))
        }
      }
      else {
        y
      }
    }
    loop(x, Nil)
  }

  def main(args: Array[String]): Unit = {
    val i = fromBinaryToInt(One(One(Zero(Nil))))
    val b = fromIntToBinary(4)
    val c = fromIntToBinary(5)
    println(b)
    println(c)
  }
}