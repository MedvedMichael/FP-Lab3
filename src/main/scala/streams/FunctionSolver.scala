package streams

import scala.annotation.tailrec

object FunctionSolver extends App {
  def calculateFunction5: PartialFunction[(Double, Double), Double] = {
    case (x, k) if x != 10 =>
      if (x > 10)
        sum(x, 1, 8)
      else
        k * scala.math.pow(x, k)
  }

  def sum(x: Double, start: Int, end: Int): Double = {
    @tailrec
    def sumRecurs(x: Double, start: Int, end: Int, result: Double): Double =
      if (start == end)
        result + x * start
      else
        sumRecurs(x, start + 1, end, result + x * start)

    sumRecurs(x, start, end, 0)
  }


  def toList(range: Seq[Int], k: Double): List[Double] =
    range.map(x => (x.toDouble, k)).collect(calculateFunction5).toList


  val list = toList(-250 to 250, 2)

  println(list)
  println(list.filter(num => num < 1000))
  println(list.foldLeft(0.0) {
    case (acc, num) => acc + num
  })
  println(list.indexWhere(num => num == 0))
  println(list.exists(num => num % 3 == 0))
  print(list.find(num => num < 135))


}
