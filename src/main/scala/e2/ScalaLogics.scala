package e2

import util.Sequences.Sequence
import util.Streams.*

trait ScalaLogics:
  def size: Int

  def win: Boolean

  def lose: Boolean

  def hit(x: Int, y: Int): Unit

  def grid: ScalaGrid

trait ScalaCell:
  def cellType: CellType

  def value: String

  def triggered: Boolean

trait ScalaGrid:
  def size: Int

  def cells: Sequence[ScalaCell]

  def cell(x: Int, y: Int): ScalaCell

  def updateCell(x: Int, y: Int, cell: ScalaCell): ScalaGrid

object ScalaCell:
  def apply(cellType: CellType, value: String, triggered: Boolean): ScalaCell =
    CellImpl(cellType, value, triggered)

  private case class CellImpl(
                               cellType: CellType,
                               value: String,
                               triggered: Boolean = false
                             ) extends ScalaCell

object ScalaGrid:
  def apply(size: Int, cells: Sequence[ScalaCell]): ScalaGrid =
    GridImpl(size, cells)

  private case class GridImpl(size: Int, var cells: Sequence[ScalaCell])
    extends ScalaGrid:
    def cell(x: Int, y: Int): ScalaCell =
      var c: ScalaCell = null
      var index = 0
      cells.map(cell =>
        if index == x * size + y then c = cell
        index += 1
      )
      c

    def updateCell(x: Int, y: Int, cell: ScalaCell): ScalaGrid =
      var index = 0
      cells = cells.map(c =>
        if index == x * size + y then
          index += 1
          cell
        else
          index += 1
          c
      )
      ScalaGrid(size, cells)

object ScalaLogics:
  def apply(size: Int): ScalaLogics =
    LogicsImpl(size)

  private case class LogicsImpl(size: Int) extends ScalaLogics:
    var grid: ScalaGrid = ScalaGrid(
      size,
      initGrid
    )
    grid = calculateValues

    def hit(x: Int, y: Int): Unit =
      val cell = grid.cell(x, y)
      if cell.cellType == CellType.MINE then
        grid = grid.updateCell(x, y, ScalaCell(CellType.MINE, "X", true))
      else
        grid = grid.updateCell(x, y, ScalaCell(CellType.SAFE, cell.value, true))
        grid = recursiveHit(x, y)

    private def recursiveHit(x: Int, y: Int): ScalaGrid =
      for i <- -1 to 1 do
        for j <- -1 to 1 do
          val newX = x + i
          val newY = y + j
          if newX >= 0 && newX < size && newY >= 0 && newY < size then
            val cell = grid.cell(newX, newY)
            if !cell.triggered && cell.cellType == CellType.SAFE && cell.value == "" then
              grid = grid.updateCell(newX, newY, ScalaCell(CellType.SAFE, cell.value, true))
              recursiveHit(newX, newY)
      grid

    def lose: Boolean = !grid.cells
      .find(c => c.triggered && c.cellType == CellType.MINE)
      .isEmpty

    def win: Boolean =
      val safeTriggeredCells =
        grid.cells.filter(c => c.cellType == CellType.SAFE && c.triggered)
      val mineCells = grid.cells.filter(c => c.cellType == CellType.MINE)
      var numberOfMines = 0
      var numberOfSafe = 0
      mineCells.map(_ => numberOfMines += 1)
      safeTriggeredCells.map(_ => numberOfSafe += 1)
      numberOfSafe == size * size - numberOfMines

    private def calculateValues: ScalaGrid =
      var newCells: Sequence[ScalaCell] = Sequence.empty
      for x <- 0 until size do
        for y <- 0 until size do
          val cell = grid.cell(x, y)
          val value = cell.cellType match
            case CellType.MINE =>
              "X"
            case CellType.SAFE =>
              val mines = countNearestMines(x, y)
              if mines == 0 then ""
              else
                mines.toString
          newCells = newCells.concat(
            Sequence(ScalaCell(cell.cellType, value, cell.triggered))
          )
      ScalaGrid(size, newCells)

    private def countNearestMines(x: Int, y: Int): Int =
      var count = 0
      for i <- -1 to 1 do
        for j <- -1 to 1 do
          if i != 0 || j != 0 then
            val newX = x + i
            val newY = y + j
            if newX >= 0 && newX < size && newY >= 0 && newY < size then
              val cell = grid.cell(newX, newY)
              if cell.cellType == CellType.MINE then count += 1
      count

    private def initGrid: Sequence[ScalaCell] =
      val random = new scala.util.Random
      var cells: Sequence[ScalaCell] = Sequence.empty
      for _ <- 0 until size * size do
        cells = cells.concat(
          Sequence(
            ScalaCell(
              random.nextInt(20) match
                case x if x < 2 => CellType.MINE
                case _ => CellType.SAFE
              ,
              "A",
              false
            )
          )
        )
      cells

