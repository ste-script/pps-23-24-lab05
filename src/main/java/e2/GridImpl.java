package e2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class GridImpl implements Grid {

    private int gridSize;
    private List<Cell> grid;

    public GridImpl(int gridSize, int numberOfMines) {
        this.gridSize = gridSize;
        grid = new ArrayList<>();
        createOrderedGrid(numberOfMines);
    }

    @Override
    public Cell getCell(Pair<Integer, Integer> position) {
        return grid.get(position.getX() * gridSize + position.getY());
    }

    @Override
    public void randomizeGrid() {
        Collections.shuffle(grid);
    }

    @Override
    public int getNumberOfAdiacentMines(Pair<Integer, Integer> position) {
        int mineCounter = 0;
        for (int i = position.getX() - 1; i <= position.getX() + 1; i++) {
            for (int j = position.getY() - 1; j <= position.getY() + 1; j++) {
                if (i >= 0 && i < gridSize && j >= 0 && j < gridSize) {
                    mineCounter += grid.get(i * gridSize + j).isMine() ? 1 : 0;
                }
            }
        }
        return mineCounter;
    }

    private void createOrderedGrid(int numberOfMines) {
        for (int i = 0; i < gridSize; i++) {
            for (int j = 0; j < gridSize; j++) {
                if (numberOfMines > 0) {
                    grid.add(new CellImpl(CellType.MINE, ""));
                    numberOfMines--;
                } else {
                    grid.add(new CellImpl(CellType.SAFE, ""));
                }
            }
        }
    }

    @Override
    public Iterator<Cell> iterator() {
        return grid.iterator();
    }

}
