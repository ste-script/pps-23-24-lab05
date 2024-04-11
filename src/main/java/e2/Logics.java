package e2;

public interface Logics {

    Grid getGrid();

    boolean isWinCondition();

    boolean isLoseCondition();

    void triggerCell(Pair<Integer, Integer> position);

}