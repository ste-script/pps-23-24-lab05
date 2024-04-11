package e2;

public class LogicsImpl implements Logics {

    private final Grid grid;
    private final int size;
    private final GameLogicDifficulty difficulty;
    private final int numberOfMines;

    public LogicsImpl(int size, GameLogicDifficulty difficulty) {
        this.size = size;
        this.difficulty = difficulty;
        this.numberOfMines = calculateNumberOfMines();
        this.grid = new GridImpl(size, this.numberOfMines);
        this.grid.randomizeGrid();
        writeAdiacents();
    }

    public LogicsImpl(int size, int numberOfMines) {
        this.size = size;
        this.difficulty = GameLogicDifficulty.EASY;
        this.numberOfMines = numberOfMines;
        checkValidNumberOfMines();
        this.grid = new GridImpl(size, numberOfMines);
        writeAdiacents();
    }

    private void checkValidNumberOfMines() {
        if (this.numberOfMines <= 0 || this.numberOfMines > (this.size * this.size) / 2) {
            throw new IllegalArgumentException("Invalid number of mines");
        }
    }

    private int calculateNumberOfMines() {
        return ((this.size * this.size) / 2) * difficulty.getValue() / 100;
    }

    @Override
    public Grid getGrid() {
        return this.grid;
    }

    @Override
    public boolean isWinCondition() {
        int safeCounter = 0;
        for (var cell : this.grid) {
            if (!cell.isMine() && cell.isTriggered()) {
                safeCounter++;
            }
        }
        return safeCounter == (this.size * this.size) - this.numberOfMines;
    }

    @Override
    public boolean isLoseCondition() {
        for (var cell : this.grid) {
            if (cell.isMine() && cell.isTriggered()) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void triggerCell(Pair<Integer, Integer> position) {
        var cell = this.grid.getCell(position);
        cell.trigger();
        if (!cell.isMine()) {
            recursiveTriggerAdiacentCells(position);
        }
    }

    private void writeAdiacents() {
        for (int i = 0; i < this.size; i++) {
            for (int j = 0; j < this.size; j++) {
                var cell = this.grid.getCell(new Pair<>(i, j));
                if (!cell.isMine()) {
                    cell.setText(String.valueOf(this.grid.getNumberOfAdiacentMines(new Pair<>(i, j))));
                } else {
                    cell.setText("*");
                }
            }
        }
    }

    private void recursiveTriggerAdiacentCells(Pair<Integer, Integer> position) {
        for (int i = position.getX() - 1; i <= position.getX() + 1; i++) {
            for (int j = position.getY() - 1; j <= position.getY() + 1; j++) {
                if (i >= 0 && i < this.size && j >= 0 && j < this.size) {
                    var cellPosition = new Pair<>(i, j);
                    var cell = this.grid.getCell(cellPosition);
                    if (!cell.isTriggered() && !cell.isMine()) {
                        cell.trigger();
                        if (grid.getNumberOfAdiacentMines(new Pair<Integer, Integer>(i, j)) == 0) {
                            recursiveTriggerAdiacentCells(cellPosition);
                        }
                    }
                }
            }
        }
    }
}
