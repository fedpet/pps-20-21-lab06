package u06lab.solution

import org.junit.jupiter.api.Assertions.{assertEquals, assertTrue}
import org.junit.jupiter.api.Test
import u06lab.solution.TicTacToe.{Board, Game, Mark, O, X, computeAnyGame, find, placeAnyMark, winner}

class TicTacToeTest {
  @Test def testFind(): Unit = {
    assertEquals(Some(X), find(List(Mark(0,0,X)),0,0))
    assertEquals(Some(O), find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1))
    assertEquals(None, find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1))
  }

  @Test def testPlaceAnyMark(): Unit = {
    val expected:Game = List(
      List(Mark(0,1,X), Mark(0,0,O)),
      List(Mark(0,2,X), Mark(0,0,O)),
      List(Mark(1,0,X), Mark(0,0,O)),
      List(Mark(1,1,X), Mark(0,0,O)),
      List(Mark(1,2,X), Mark(0,0,O)),
      List(Mark(2,0,X), Mark(0,0,O)),
      List(Mark(2,1,X), Mark(0,0,O)),
      List(Mark(2,2,X), Mark(0,0,O))
    )
    assertEquals(expected, placeAnyMark(List(Mark(0,0,O)),X))
  }

  @Test def testWinner(): Unit = {
    assertEquals(None, winner(List()))
    assertEquals(None, winner(List(Mark(0,0,X), Mark(1,0,O), Mark(2,0,X))))
    assertEquals(Some(X), winner(List(Mark(0,0,X), Mark(1,0,X), Mark(2,0,X))))
    assertEquals(Some(X), winner(List(Mark(2,2,X), Mark(1,1,X), Mark(0,0,X))))
    assertEquals(None, winner(List(Mark(2,2,X), Mark(1,1,X), Mark(0,0,O))))
  }
}
