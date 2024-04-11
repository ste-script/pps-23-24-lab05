package e2;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class LogicsTest {

    private Logics logics;
    private int numberOfMines;
    private int size = 8;

    @BeforeEach
    public void setUp() {
        this.numberOfMines = 1;
        this.logics = new LogicsImpl(size, numberOfMines);
    }

    @Test
    public void getGrid() {
        var grid = logics.getGrid();
        int sizeCounter = 0;
        for (var cell : grid) {
            sizeCounter++;
        }
        assertEquals(sizeCounter, size * size);
    }

    @Test
    public void invalidNumberOfMines() {
        assertThrows(IllegalArgumentException.class, () -> new LogicsImpl(8, 0));
    }

    @Test
    public void isWinCondition() {
        this.logics.triggerCell(new Pair<>(0, 1));
        assertTrue(this.logics.isWinCondition());
    }

    @Test
    public void isLoseCondition() {
        this.logics.triggerCell(new Pair<>(0, 0));
        assertTrue(this.logics.isLoseCondition());
    }

    @Test
    public void correctAdiacentValuesHorizontal() {
        var localGrid = this.logics.getGrid();
        var cell = localGrid.getCell(new Pair<>(1, 0));
        assertEquals(cell.getText(), "1");
    }

    @Test
    public void correctAdiacentValuesVertical() {
        var localGrid = this.logics.getGrid();
        var cell = localGrid.getCell(new Pair<>(0, 1));
        assertEquals(cell.getText(), "1");
    }

    @Test
    public void correctAdiacentValuesDiagonal() {
        var localGrid = this.logics.getGrid();
        var cell = localGrid.getCell(new Pair<>(1, 1));
        assertEquals(cell.getText(), "1");
    }

    @Test
    public void correctAdiacentValuesNoMine() {
        var localGrid = this.logics.getGrid();
        var cell = localGrid.getCell(new Pair<>(2, 1));
        assertEquals(cell.getText(), "0");
    }
}
