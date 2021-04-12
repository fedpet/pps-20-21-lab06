package u06lab.solution

object TicTacToe {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(p => p.x == x && p.y == y).map(_.player)

  def placeAnyMark(board: Board, player: Player): Seq[Board] = for(
    x <- 0 to 2;
    y <- 0 to 2;
    mark = Mark(x, y, player)
    if find(board, x, y).isEmpty
  ) yield mark :: board

  def winner(board: Board): Option[Player] = board.groupBy(_.player).values.find(marks =>
      marks.count(m => m.x == m.y) >= 3 // diagonal
      || marks.count(m => m.x == 2 - m.y) >= 3 // other diagonal
      || marks.groupBy(_.x).values.size >= 3 // columns
      || marks.groupBy(_.y).values.size >= 3 // rows
    ).map(_.head.player)

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case n if n > 0 => for (
      game <- computeAnyGame(player.other, moves - 1);
      nextBoard <- placeAnyMark(game.head, player)
      if winner(game.head).isEmpty
    ) yield nextBoard :: game
    case _ => Stream[Game](List[Board](Nil))
  }

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }
}
