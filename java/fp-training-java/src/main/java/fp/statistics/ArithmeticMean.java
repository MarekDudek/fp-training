package fp.statistics;

public final class ArithmeticMean {

    private final double sum;
    private final int count;

    private ArithmeticMean(double s, int c) {
        sum = s;
        count = c;
    }

    public static ArithmeticMean arithmeticMean(double number) {
        return new ArithmeticMean(number, 1);
    }

    public double getArithmeticMean() {
        return sum / count;
    }

    public static ArithmeticMean append(ArithmeticMean a, ArithmeticMean b) {
        return new ArithmeticMean(a.sum + b.sum, a.count + b.count);
    }
}
