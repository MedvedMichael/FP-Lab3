package streams

class BloxorzSuite extends munit.FunSuite {
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  test("terrain function level 1 (10pts)") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("find char level 1 (10pts)") {
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }
  }


  test("optimal solution for level 1 (5pts)") {
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }
  }


  test("optimal solution length for level 1 (5pts)") {
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }
  }

  test("correct finds neighbors") {
    new Level1 {
      assertEquals(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
        , Set(
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)),
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up))
        ).to(LazyList)
      )
    }

  }

  test("no cycles in paths") {
    new Level1 {
      assertEquals(newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).to(LazyList),
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
      ), Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ).to(LazyList)
      )

    }
  }

  import scala.concurrent.duration._

  override val munitTimeout: FiniteDuration = 10.seconds
}
