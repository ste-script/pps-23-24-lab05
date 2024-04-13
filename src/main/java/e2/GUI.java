package e2;

import javax.swing.*;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.MouseInputListener;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

public class GUI extends JFrame {

    private static final long serialVersionUID = -6218820567019985015L;
    private final Map<JButton, Pair<Integer, Integer>> buttons = new HashMap<>();
    private final ScalaLogics logics;

    public GUI(int size) {
        this.logics = ScalaLogics.apply(size);
        //this.logics = new LogicsImpl(size, GameLogicDifficulty.EASY);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setSize(100 * size, 100 * size);

        JPanel panel = new JPanel(new GridLayout(size, size));
        this.getContentPane().add(BorderLayout.CENTER, panel);

        ActionListener onClick = (e) -> {
            final JButton bt = (JButton) e.getSource();
            final Pair<Integer, Integer> pos = buttons.get(bt);
            logics.hit(pos.getX(), pos.getY());
            if (logics.lose()) {
                quitGame();
                JOptionPane.showMessageDialog(this, "You lost!!");
            } else {
                drawBoard();
            }
            if (logics.win()) {
                quitGame();
                JOptionPane.showMessageDialog(this, "You won!!");
                System.exit(0);
            }
        };

        MouseInputListener onRightClick = new MouseInputAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                final JButton bt = (JButton) e.getSource();
                if (bt.isEnabled()) {
                    final Pair<Integer, Integer> pos = buttons.get(bt);
                    if (bt.getText().equals("F")) {
                        bt.setText(" ");
                    } else {
                        bt.setText("F");
                    }
                }
                drawBoard();
            }
        };

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                final JButton jb = new JButton(" ");
                jb.addActionListener(onClick);
                jb.addMouseListener(onRightClick);
                this.buttons.put(jb, new Pair<>(i, j));
                panel.add(jb);
            }
        }
        this.drawBoard();
        this.setVisible(true);
    }

    private void quitGame() {
        for (var entry : this.buttons.entrySet()) {
            filterGrid(entry, (cell) -> true);
        }
    }

    private void drawBoard() {
        for (var entry : this.buttons.entrySet()) {
            filterGrid(entry, ScalaCell::triggered);
        }
    }

    private void filterGrid(Map.Entry<JButton, Pair<Integer, Integer>> entry, Predicate<ScalaCell> predicate) {
        var cell = logics.cell(entry.getValue().getX(), entry.getValue().getY());
        if (predicate.test(cell)) {
            entry.getKey().setText(cell.value());
            entry.getKey().setEnabled(false);
        }
    }

}
