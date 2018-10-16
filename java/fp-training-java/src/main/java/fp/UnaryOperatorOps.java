package fp;

import java.util.function.UnaryOperator;

public enum UnaryOperatorOps {

    ;

    public static <T> UnaryOperator<T> compose(final UnaryOperator<T> g, final UnaryOperator<T> f) {
        return t -> g.apply(f.apply(t));
    }

    public static <T> UnaryOperator<T> andThen(final UnaryOperator<T> f, final UnaryOperator<T> g) {
        return t -> g.apply(f.apply(t));
    }
}
