package fp;

import java.util.function.Function;

public interface Endo<A> extends Function<A, A> {

    default Endo<A> andThen(final Endo<A> after) {
        return a -> after.apply(apply(a));
    }

    default Endo<A> compose(final Endo<A> before) {
        return a -> apply(before.apply(a));
    }
}
