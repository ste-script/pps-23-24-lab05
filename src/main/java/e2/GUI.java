package e2;

import javax.swing.*;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.MouseInputListener;

import java.util.*;
import java.util.Map.Entry;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

public class GUI extends JFrame {

	private static final long serialVersionUID = -6218820567019985015L;
	private final Map<JButton, Pair<Integer, Integer>> buttons = new HashMap<>();
	private final Logics logics;

	public GUI(int size) {
		this.logics = ScalaLogics.apply(size, GameLogicDifficulty.EASY);
		//this.logics = new LogicsImpl(size, GameLogicDifficulty.EASY);
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		this.setSize(100 * size, 100 * size);

		JPanel panel = new JPanel(new GridLayout(size, size));
		this.getContentPane().add(BorderLayout.CENTER, panel);

		ActionListener onClick = (e) -> {
			final JButton bt = (JButton) e.getSource();
			final Pair<Integer, Integer> pos = buttons.get(bt);
			logics.triggerCell(pos);
			if (logics.isLoseCondition()) {
				quitGame();
				JOptionPane.showMessageDialog(this, "You lost!!");
			} else {
				drawBoard();
			}
			if (logics.isWinCondition()) {
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
		var grid = logics.getGrid();
		for (var entry : this.buttons.entrySet()) {
			// call the logic here
			// if this button is a mine, draw it "*"
			// disable the button
			writeValueAndDisable(grid, entry);
		}
	}

	private void writeValueAndDisable(Grid grid, Entry<JButton, Pair<Integer, Integer>> entry) {
		entry.getKey().setText(grid.getCell(entry.getValue()).getText());
		entry.getKey().setEnabled(false);
	}

	private void drawBoard() {
		var grid = logics.getGrid();
		for (var entry : this.buttons.entrySet()) {
			// call the logic here
			// if this button is a cell with counter, put the number
			// if this button has a flag, put the flag
			if (grid.getCell(entry.getValue()).isTriggered()) {
				writeValueAndDisable(grid, entry);
			}
		}
	}

}
