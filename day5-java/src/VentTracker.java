import lombok.Value;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class VentTracker {

    private final int diagramSize;
    private final List<Trajectory> lines = new ArrayList<>();

    @Value
    private static class Point {
        int x;
        int y;

        private static Point fromString(String pStr) {
            String[] coords = pStr.split(",");
            return new Point(Integer.parseInt(coords[0]), Integer.parseInt(coords[1]));
        }

        public int maxVal() {
            return Math.max(x, y);
        }
    }

    @Value
    private static class Trajectory {
        Point start;
        Point end;

        private static Trajectory fromString(String tStr) {
            String[] points = tStr.split(" -> ");
            return new Trajectory(Point.fromString(points[0]), Point.fromString(points[1]));
        }

        public boolean isVertical() {
            return start.getX() == end.getX();
        }

        public boolean isHorizontal() {
            return start.getY() == end.getY();
        }

        public List<Point> getSpan() {
            int currX = start.getX();
            int currY = start.getY();
            int dx = Integer.compare(end.getX(), start.getX());
            int dy = Integer.compare(end.getY(), start.getY());

            List<Point> span = new ArrayList<>();

            while (currX != end.getX() || currY != end.getY()) {
                span.add(new Point(currX, currY));
                currX += dx;
                currY += dy;
            }

            span.add(end);
            return span;
        }

        public int maxVal() {
            return Math.max(start.maxVal(), end.maxVal());
        }
    }

    public VentTracker(String pathname) {
        try {
            File inputFile = new File(pathname);
            Scanner reader = new Scanner(inputFile);
            int maxVal = 0;
            while (reader.hasNextLine()) {
                Trajectory line = Trajectory.fromString(reader.nextLine());
                maxVal = Math.max(maxVal, line.maxVal());
                lines.add(line);
            }
            diagramSize = maxVal + 1;
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Input file not found.", e);
        }
    }

    public int findOverlap(boolean diagonalsAllowed) {
        int[][] diagram = new int[this.diagramSize][this.diagramSize];
        int numOverlaps = 0;

        for (Trajectory line : lines) {
            if (diagonalsAllowed || line.isHorizontal() || line.isVertical()) {
                List<Point> span = line.getSpan();
                for (Point p : span) {
                    int here = diagram[p.getY()][p.getX()]++;
                    if (here == 1) {
                        // Value was previously 1, so now 2.
                        numOverlaps++;
                    }
                }
            }
        }

        return numOverlaps;
    }
}
