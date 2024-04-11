package e2

trait ScalaLogics extends Logics

object ScalaLogics:

  def apply(size: Int, difficulty: GameLogicDifficulty): ScalaLogics =
    val numberOfMines = ((size * size) / 2) * difficulty.getValue() / 100
    ScalaLogicsImpl(size, numberOfMines)

  def apply(size: Int, numberOfMines: Int): ScalaLogics =
    ScalaLogicsImpl(size, numberOfMines)

  private case class ScalaLogicsImpl(
      val size: Int,
      val numberOfMines: Int
  ) extends ScalaLogics:
    checkValidNumberOfMines()
    val grid = GridImpl(size, numberOfMines)
    grid.randomizeGrid()
    writeAdiacents()
    def getGrid(): Grid = grid
    def triggerCell(position: e2.Pair[Integer, Integer]): Unit =
      val cell = grid.getCell(position)
      cell.trigger()
      if !cell.isMine() then recursiveTriggerAdiacentCells(position)
    def isWinCondition(): Boolean =
      var safeCounter: Int = 0
      grid.forEach((cell: Cell) =>
        if !cell.isMine() && cell.isTriggered() then
          safeCounter = safeCounter + 1
      )
      safeCounter == ((size * size) - numberOfMines)

    def isLoseCondition(): Boolean =
      var isLose = false
      grid.forEach((cell: Cell) =>
        if cell.isMine() && cell.isTriggered() then isLose = true
      )
      isLose

    private def writeAdiacents(): Unit =
      for i: Int <- 0 to size - 1 do
        for j: Int <- 0 to size - 1 do
          var position = e2.Pair(Integer(i), Integer(j))
          var cell: Cell = grid.getCell(position)
          if cell.isMine() then cell.setText("*")
          else
            cell.setText(
              grid
                .getNumberOfAdiacentMines(position)
                .toString()
            )

    private def checkValidNumberOfMines(): Unit =
      if this.numberOfMines <= 0 || this.numberOfMines > (this.size * this.size) / 2
      then throw new IllegalArgumentException("Invalid number of mines");

    private def countRemainingSafeCells(): Int =
      var counter = 0
      grid.forEach((cell: Cell) =>
        if !cell.isMine() && cell.isTriggered() then counter = counter + 1
      )
      counter

    private def isValidPosition(position: e2.Pair[Integer, Integer]): Boolean =
      position.getX() >= 0 && position.getX() < size && position
        .getY() >= 0 && position.getY() < size

    private def recursiveTriggerAdiacentCells(
        position: e2.Pair[Integer, Integer]
    ): Unit =
      val xPos = position.getX()
      for i: Int <- xPos - 1 to xPos + 1 do
        val yPos = position.getY()
        for j: Int <- yPos - 1 to yPos + 1 do
          val newPosition = e2.Pair(Integer(i), Integer(j))
          if isValidPosition(newPosition) then
            val cell = grid.getCell(newPosition)
            if !cell.isMine() && !cell.isTriggered() then
              cell.trigger()
              if grid.getNumberOfAdiacentMines(newPosition) == 0 then
                recursiveTriggerAdiacentCells(newPosition)

@main def test() = ScalaLogics(10, 10)
