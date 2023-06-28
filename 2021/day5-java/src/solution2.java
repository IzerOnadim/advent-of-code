public class solution2 {
    public static void main(String[] args) {
        VentTracker vents = new VentTracker("input.txt");
        System.out.println(vents.findOverlap(true)); // Diagonals allowed.
    }
}
