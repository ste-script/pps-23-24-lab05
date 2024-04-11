package e2;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class LogicsTest {

    private Logics logics;
    private GameLogicDifficulty difficulty = GameLogicDifficulty.EASY;
    private int size = 8;

    @BeforeEach
    public void setUp() {
        this.logics = new LogicsImpl(size, difficulty);
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
        var localLogics = new LogicsImpl(8, 1);
        localLogics.triggerCell(new Pair<>(0, 1));
        assertTrue(localLogics.isWinCondition());
    }

    @Test
    public void isLoseCondition() {
        var localLogics = new LogicsImpl(8, 1);
        localLogics.triggerCell(new Pair<>(0, 0));
        assertTrue(localLogics.isLoseCondition());
    }

    @Test
    public void correctAdiacentValuesHorizontal() {
        var localLogics = new LogicsImpl(8, 1);
        var localGrid = localLogics.getGrid();
        var cell = localGrid.getCell(new Pair<>(1, 0));
        assertEquals(cell.getText(), "1");
    }

    @Test
    public void correctAdiacentValuesVertical() {
        var localLogics = new LogicsImpl(8, 1);
        var localGrid = localLogics.getGrid();
        var cell = localGrid.getCell(new Pair<>(0, 1));
        assertEquals(cell.getText(), "1");
    }

    @Test
    public void correctAdiacentValuesDiagonal() {
        var localLogics = new LogicsImpl(8, 1);
        var localGrid = localLogics.getGrid();
        var cell = localGrid.getCell(new Pair<>(1, 1));
        assertEquals(cell.getText(), "1");
    }

    @Test
    public void correctAdiacentValuesNoMine() {
        var localLogics = new LogicsImpl(8, 1);
        var localGrid = localLogics.getGrid();
        var cell = localGrid.getCell(new Pair<>(2, 1));
        assertEquals(cell.getText(), "0");
    }
}
