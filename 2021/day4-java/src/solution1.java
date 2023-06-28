public class solution1 {
    public static void main(String[] args) {
        BingoPlayer player = new BingoPlayer("input.txt");
        System.out.println(player.bestBoardScore());
    }
}
