package fp;

public enum MathUtil {

    ;

    public static boolean even(int n) {
        return n % 2 == 0;
    }

    public static boolean odd(int n) {
        return !even(n);
    }
}
