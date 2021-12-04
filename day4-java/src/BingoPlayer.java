import lombok.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

public class BingoPlayer {

    private final List<Integer> numbers;
    private final List<Board> boards = new ArrayList<>();

    public BingoPlayer(String pathname) {
        try {
            File inputFile = new File(pathname);
            Scanner fileReader = new Scanner(inputFile);

            String numberLine = fileReader.nextLine();
            fileReader.nextLine(); // Discard blank line.
            numbers = Arrays.stream(numberLine.split(","))
                    .map(Integer::parseInt).collect(Collectors.toList());

            List<String> boardStr = new ArrayList<>();
            while (fileReader.hasNextLine()) {
                String line = fileReader.nextLine();
                if (line.isBlank()) {
                    boards.add(new Board(boardStr));
                    boardStr.clear();
                } else {
                    boardStr.add(line);
                }
            }
            boards.add(new Board(boardStr));

            fileReader.close();
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Input file not found", e);
        }
    }

    public int bestBoardScore() {
        // Adding, checking and calculating the score of the board are all O(1). So getting the best board score
        // is at worst O(n x m) where n is the number of bingo numbers drawn, and m is the number of boards.
        for (int number : numbers) {
            for (Board board : boards) {
                board.addNumber(number);
                if (board.checkBingo()) {
                    return board.bingoScore(number);
                }
            }
        }
        return -1; // No bingo.
    }

    @Value
    private static class Pos {
        int x;
        int y;
        boolean drawn;
    }

    private static class Board {

        private static final int BOARD_SIZE = 5;
        private final Map<Integer, Pos> board = new HashMap<>();
        private final Boolean[][] drawn = new Boolean[BOARD_SIZE][BOARD_SIZE];

        public Board(List<String> boardStr) {
            for (int i = 0; i < boardStr.size(); i++) {
                String row = boardStr.get(i);
                String[] columns = Arrays.stream(row.split(" +"))
                        .filter(x -> !x.isBlank()).toArray(String[]::new);

                for (int j = 0; j < columns.length; j++) {
                    board.put(Integer.parseInt(columns[j]), new Pos(i, j, false));
                    drawn[i][j] = false;
                }
            }
        }

        public void addNumber(int number) {
            // Adding a number is O(1) since both array and hashmap access is O(1).
            Pos numPos = board.get(number);
            if (numPos != null) {
                drawn[numPos.getX()][numPos.getY()] = true;
                board.replace(number, new Pos(numPos.getX(), numPos.getY(), true));
            }
        }

        public boolean checkBingo() {
            // This is actually constant time since BOARD_SIZE = 5. So it is 0(25) ~ O(1).

            // Check for bingo in rows.
            for (Boolean[] row : drawn) {
                if (Arrays.stream(row).reduce(true, (l, r) -> l && r)) {
                    return true;
                }
            }

            // Check for bingo in columns.
            for (int j = 0; j < BOARD_SIZE; j++) {
                boolean allTrue = true;

                for (int i = 0; i < BOARD_SIZE; i++) {
                    if (!drawn[i][j]) {
                        allTrue = false;
                        break;
                    }
                }

                if (allTrue) {
                    return true;
                }
            }

            return false;
        }

        private int bingoScore(int lastNum) {
            // This is also constant time since there are at most BOARD_SIZE x BOARD_SIZE elements in the hashmap.
            // So O(BOARD_SIZE^2) = O(25) ~ O(1).
            AtomicInteger bingoSum = new AtomicInteger();
            board.forEach((k, v) -> {
                if (!v.isDrawn()) {
                    bingoSum.getAndAdd(k);
                }
            });
            return lastNum * bingoSum.get();
        }
    }
}

