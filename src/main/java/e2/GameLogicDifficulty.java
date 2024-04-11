package e2;

public enum GameLogicDifficulty {
    EASY(20), MEDIUM(50), HARD(90);

    private int size;

    GameLogicDifficulty(int size) {
        this.size = size;
    }

    public int getValue() {
        return size;
    }
}