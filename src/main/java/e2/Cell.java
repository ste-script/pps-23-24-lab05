package e2;

public interface Cell {

    String getText();

    void setText(String text);

    CellType getType();

    boolean isMine();

    boolean isTriggered();

    void trigger();

}
