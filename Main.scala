
sealed trait Direction
case object Horizontal extends Direction
case object Vertical extends Direction
object Direction {
  def random(): Direction =
    if (Math.random() < 0.5)
      Horizontal
    else
      Vertical
}

case class Position(x: Int, y: Int) {
  def offset(direction: Direction, steps: Int): Position =
    direction match {
      case Horizontal => copy(y = y + steps)
      case Vertical => copy(x = x + steps)
    }
  def surrounding: List[Position] =
    List(
      Position(x - 1, y - 1),
      Position(x - 1, y    ),
      Position(x - 1, y + 1),
      Position(x    , y - 1),
      Position(x    , y + 1),
      Position(x + 1, y - 1),
      Position(x + 1, y    ),
      Position(x + 1, y + 1)
    )
}

case class Ship(size: Int, plane: Direction, position: Position)

case class Cell(hidden: Boolean, containsShipElement: Boolean, isBoundary: Boolean) {
  def fill: Cell = copy(containsShipElement = true)
  def reveal: Cell = copy(hidden = false)
  def isRevealed: Boolean = !containsShipElement
  def setBoundary: Cell = copy(isBoundary = true)
  def placeShip(ship: Ship): Cell = {
    copy(isBoundary = true, containsShipElement = true)
  }
}
object Cell {
  def empty: Cell = Cell(hidden = true, containsShipElement = false, isBoundary = false)
}

case class Board(ships: Map[Ship, (List[Cell], List[Cell])], cells: Array[Array[Cell]], width: Int, height: Int) {

  def getCell(position: Position):Cell =
    cells(position.x)(position.y)

  def setCell(position: Position, cell: Cell): Unit =
    cells(position.x)(position.y) = cell

  def surrounding(position: Position): List[Cell] =
    position.surrounding.filter(inside).map(getCell)

  def playerString: String =
    cells.map(_.map(cell =>
      if (cell.containsShipElement) "X" else " "
    ).mkString("|")).mkString("\n")

  def enemyString: String =
    cells.map(_.map(cell =>
      if (cell.hidden) "?" else if (cell.containsShipElement) "X" else " "
    ).mkString("|")).mkString("\n")

  def open(position: Position): Board = {
    setCell(position, getCell(position).reveal)
    copy(cells = cells)
  }

  def inside(position: Position): Boolean =
    position.x < 0 || position.x >= width || position.y < 0 || position.y >= height

  def add(ship: Ship): Board = {
    val occupiedPositions = (0 until ship.size).map(
      ship.position.offset(ship.plane, _)
    )
    occupiedPositions.foreach(p => setCell(p, getCell(p).placeShip(ship)))
    val occupiedCells = occupiedPositions.map(getCell).toList
    val boundaryCells = occupiedPositions.flatMap(surrounding).toSet.diff(occupiedCells.toSet).toList
    copy(ships = ships + (ship -> (occupiedCells, boundaryCells)), cells = cells)
  }

  def isUncovered: Boolean = {
    def hasFilled(cellRow: Array[Cell]): Boolean = cellRow.exists(!_.isRevealed)
    cells.exists(hasFilled)
  }

  def isClear: Boolean =
    !isUncovered

  def attack(position: Position): Board = {
    setCell(position, getCell(position).reveal)
    if (getCell(position).ship.isDestroyed)
      revealBorders(getCell(position).ship)
    copy(cells = cells)
  }

  def revealBorders(ship: Ship): Board = {
    ships(ship)._2.foreach()
  }


  def randomAvailablePosition(size: Int): (Position, Direction) = ???
}

object Board {
  val DEFAULT_WIDTH = 10
  val DEFAULT_HEIGHT = 10
}

object BattleshipService {
  def createBoard(width: Int = Board.DEFAULT_WIDTH, height: Int = Board.DEFAULT_HEIGHT): Board =
    Board(List(), Array.fill(width, height)(Cell.empty), width, height)

  def attack(board: Board, position: Position): Board = {
    board.
  }

  def addShip(board: Board, ship: Ship): Board = board add ship

  def defaultBoardInitialise(board: Board): Board = {
    val defaultShips = List(
      Ship(4, Horizontal, Position(9,2)),
      Ship(3, Horizontal, Position(0,1)),
      Ship(3, Vertical,   Position(5,6)),
      Ship(2, Horizontal, Position(0,8)),
      Ship(2, Vertical,   Position(5,1)),
      Ship(2, Vertical,   Position(4,3)),
      Ship(1, Horizontal, Position(0,5)),
      Ship(1, Horizontal, Position(3,0)),
      Ship(1, Horizontal, Position(3,7)),
      Ship(1, Horizontal, Position(7,4))
    )
    var newBoard: Board = board
    defaultShips.foreach(ship => newBoard = newBoard add ship)
    newBoard
  }

  def announceWinner(player1: Player, player2: Player): Unit = {
    val winner = List(player1, player2).filter(!_.hasLost).head
    println(s"${winner.name} has won. Congratulations!")
  }
}

case class Player(name: String, board: Board, points: Int = 0) {
  def move = {
    // accept input
    // attack board
  }
  def hasLost: Boolean = board.isClear
}

case class Game(p1: Player, p2: Player) {
  def isFinished: Boolean = p1.hasLost || p2.hasLost
  def movePlayers: Game = {
    p1.move
    if (!p1.hasLost)
      p2.move
    this
  }
}


  var board1 = BattleshipService.createBoard()
  board1 = BattleshipService.defaultBoardInitialise(board1)

  var board2 = BattleshipService.createBoard()
  board2 = BattleshipService.defaultBoardInitialise(board2)

  val player1 = Player("Player 1", board1)
  val player2 = Player("Player 2", board2)

  val game = Game(player1, player2)

  Stream.continually[Game](game.movePlayers).takeWhile(!_.isFinished)

  BattleshipService.announceWinner(player1, player2)

