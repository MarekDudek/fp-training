package fp.statistics;

public final class Average {

    private final double sum;
    private final int count;

    private Average(double s, int c) {
        sum = s;
        count = c;
    }

    public static Average average(double number) {
        return new Average(number, 1);
    }

    public double getAverage() {
        return sum / count;
    }

    public static Average append(Average a, Average b) {
        return new Average(a.sum + b.sum, a.count + b.count);
    }
}
