package e2;

public class CellImpl implements Cell {

    private final CellType type;
    private String text;
    private boolean triggered;

    public CellImpl(CellType mine, String text) {
        this.type = mine;
        this.text = text;
    }

    @Override
    public String getText() {
        return this.text;
    }

    @Override
    public CellType getType() {
        return this.type;
    }

    @Override
    public boolean isMine() {
        return this.type == CellType.MINE;
    }

    @Override
    public boolean isTriggered() {
        return this.triggered;
    }

    @Override
    public void trigger() {
        this.triggered = true;
    }

    @Override
    public void setText(String text) {
        this.text = text;
    }

}
