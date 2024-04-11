package e2;

public interface Grid extends Iterable<Cell> {

    Cell getCell(Pair<Integer, Integer> position);

    void randomizeGrid();

    int getNumberOfAdiacentMines(Pair<Integer, Integer> position);

}
