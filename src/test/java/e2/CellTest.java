package e2;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class CellTest {

    @Test
    public void createBombCell() {
        Cell cell = new CellImpl(CellType.MINE, "X");
        assertEquals(CellType.MINE, cell.getType());
    }

    @Test
    public void createSafeCell() {
        Cell cell = new CellImpl(CellType.SAFE, "0");
        assertEquals(CellType.SAFE, cell.getType());
    }

    @Test
    public void getCellText() {
        Cell cell = new CellImpl(CellType.SAFE, "0");
        assertEquals("0", cell.getText());
    }

    @Test
    public void isMine() {
        Cell cell = new CellImpl(CellType.MINE, "X");
        assertEquals(true, cell.isMine());
    }

    @Test
    public void trigger() {
        Cell cell = new CellImpl(CellType.MINE, "X");
        cell.trigger();
        assertEquals(true, cell.isTriggered());
    }
}
