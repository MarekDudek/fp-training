package fp;

import java.util.function.BiFunction;
import java.util.function.BinaryOperator;

public final class FunctionalUtilities {

    public static <A, B, C> BiFunction<B, A, C> flip(final BiFunction<A, B, C> f) {
        return (b, a) -> f.apply(a, b);
    }

    public static <A> BinaryOperator<A> flip(final BinaryOperator<A> f) {
        return (b, a) -> f.apply(a, b);
    }
}
