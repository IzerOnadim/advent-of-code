import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
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

    public int playBingo() {
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

    private static class Board {
        private static final int BOARD_SIZE = 5;
        private final Integer[][] board = new Integer[BOARD_SIZE][BOARD_SIZE];
        private final Boolean[][] drawn = new Boolean[BOARD_SIZE][BOARD_SIZE];

        public Board(List<String> boardStr) {
            for (int i = 0; i < boardStr.size(); i++) {
                String row = boardStr.get(i);
                String[] columns = Arrays.stream(row.split(" +"))
                        .filter(x -> !x.isBlank()).toArray(String[]::new);

                for (int j = 0; j < columns.length; j++) {
                    board[i][j] = Integer.parseInt(columns[j]);
                    drawn[i][j] = false;
                }
            }
        }

        public void addNumber(int number) {
            for (int i = 0; i < BOARD_SIZE; i++) {
                for (int j = 0; j < BOARD_SIZE; j++) {
                    int num = board[i][j];
                    if (number == num) {
                        drawn[i][j] = true;
                    }
                }
            }
        }

        public boolean checkBingo() {
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
            int bingoSum = 0;
            for (int i = 0; i < BOARD_SIZE; i++) {
                for (int j = 0; j < BOARD_SIZE; j++) {
                    if (!drawn[i][j]) {
                        bingoSum += board[i][j];
                    }
                }
            }
            return lastNum * bingoSum;
        }

        @Override
        public String toString() {
            return Arrays.stream(board).map(a -> Arrays.deepToString(a) + "\n").collect(Collectors.joining());
        }
    }
}

