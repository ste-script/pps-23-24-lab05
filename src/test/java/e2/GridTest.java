package e2;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class GridTest {

    private Grid grid;
    private int numberOfMines = 1;
    private int gridSize = 8;

    @BeforeEach
    public void setUp() {
        grid = new GridImpl(gridSize, numberOfMines);
    }

    @Test
    public void getCell() {
        var cell = grid.getCell(new Pair<>(0, 0));
        assertTrue(cell.isMine());
    }

    @Test
    public void searchMines() {
        int mineCounter = 0;
        for (var cell : grid) {
            if (cell.isMine()) {
                mineCounter++;
            }
        }
        assertEquals(numberOfMines, mineCounter);
    }

    @Test
    public void randomizeGrid() {
        var localGrid = new GridImpl(gridSize, numberOfMines);
        var copyOfLocalGrid = new GridImpl(gridSize, numberOfMines);
        localGrid.randomizeGrid();
        boolean isDifferent = false;
        for (var cell : localGrid) {
            if (cell != copyOfLocalGrid.getCell(new Pair<>(0, 0))) {
                isDifferent = true;
                break;
            }
        }
        assertTrue(isDifferent);
    }

    @Test
    public void getNumberOfAdiacentMines() {
        var localGrid = new GridImpl(8, 3);
        assertEquals(localGrid.getNumberOfAdiacentMines(new Pair<Integer, Integer>(1, 1)), 3);
    }

}
