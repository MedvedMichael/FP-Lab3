package streams

import streams.FunctionSolver.{calculateFunction5, list}

class FunctionSolverSuite extends munit.FunSuite {
  val testFunctionSolver: FunctionSolver.type = FunctionSolver
  val list: List[Double] = testFunctionSolver.toList(-250 to 250, 2)
  test("test list length") {
    assertEquals(list.length, 500)
  }

  test("filter works correctly") {
    for (elem <- list.filter(num => num < 400))
      assert(elem < 400)
  }

  test("fold works correctly") {
    val summary = list.foldLeft(0.0) {
      case (acc, num) => acc + num
    }
    var acc: Double = 0
    for (i <- -250 to 250) {
      try {
        acc += calculateFunction5(i, 2)
      }
      catch {
        case e: scala.MatchError => acc += 0
      }
    }
    assertEquals(summary, acc)
  }

  test("indexWhere works correctly") {
    assert(list(list.indexWhere(num => num == 0)) == 0)
  }

  test("exists works correctly") {
    assert(list.exists(num => num % 3 == 0))
  }

  test("find works correctly") {
    assert(list.find(num => num < 135).get < 135)
  }
}
