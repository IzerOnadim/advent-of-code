public class solution1 {
    public static void main(String[] args) {
        VentTracker vents = new VentTracker("input.txt");
        System.out.println(vents.findOverlap(false)); // No diagonals.
    }
}
