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
    def getGrid(): Grid = grid
    def triggerCell(position: e2.Pair[Integer, Integer]): Unit =
      val cell = grid.getCell(position)
      cell.trigger()
      if !cell.isMine() then recursiveTriggerAdiacentCells(position)
    def isWinCondition(): Boolean =
      countRemainingSafeCells() == (size ^ 2) - numberOfMines

    def isLoseCondition(): Boolean =
      var isLose = false
      grid.forEach((cell: Cell) =>
        if cell.isMine() && cell.isTriggered() then isLose = true
      )
      isLose

    private def checkValidNumberOfMines(): Unit =
      if this.numberOfMines <= 0 || this.numberOfMines > (this.size * this.size) / 2
      then throw new IllegalArgumentException("Invalid number of mines");

    private def countRemainingSafeCells(): Int =
      var counter = 0
      grid.forEach((cell: Cell) =>
        if !cell.isMine() && !cell.isTriggered() then counter = counter + 1
      )
      counter

    private def isValidPosition(position: e2.Pair[Integer, Integer]): Boolean =
      position.getX() >= 0 && position.getX() < size && position
        .getY() >= 0 && position.getY() < size

    private def recursiveTriggerAdiacentCells(
        position: e2.Pair[Integer, Integer]
    ): Unit =
      for i: Int <- -1 to position.getX() + 1 do
        for j: Int <- -1 to position.getY() + 1 do
          val newPosition = e2.Pair(Integer(i), Integer(j))
          if isValidPosition(newPosition) then
            val cell = grid.getCell(newPosition)
            if !cell.isMine() && !cell.isTriggered() then
              cell.trigger()
              if grid.getNumberOfAdiacentMines(newPosition) == 0 then
                recursiveTriggerAdiacentCells(newPosition)

@main def test() = ScalaLogics(10, 10)