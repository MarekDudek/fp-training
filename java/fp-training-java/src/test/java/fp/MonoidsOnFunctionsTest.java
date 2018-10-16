package fp;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

import static java.util.function.UnaryOperator.identity;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class MonoidsOnFunctionsTest {

    @Test
    public void monoid_for_endomorphisms() {
        // given
        final UnaryOperator<String> a = s -> "Hello, ".concat(s);
        final UnaryOperator<String> b = s -> s.concat("!");
        // when
        final UnaryOperator<String> computation1 = UnaryOperatorOps.andThen(a, b);
        final UnaryOperator<String> computation2 = UnaryOperatorOps.compose(b, a);
        final UnaryOperator<String> computation3 = Stream.of(a, b).reduce(identity(), UnaryOperatorOps::andThen);
        // then
        assertThat(
                computation1.apply("World"),
                is(
                        "Hello, World!"
                )
        );
        assertThat(
                computation2.apply("World"),
                is(
                        "Hello, World!"
                )
        );
        assertThat(
                computation3.apply("World"),
                is(
                        "Hello, World!"
                )
        );
    }

    private static final Function<Integer, String> id = __ -> "";
    private static final BinaryOperator<
            Function<Integer, String>
            > append =
            (f, g) -> i -> {
                String fi = f.apply(i);
                String gi = g.apply(i);
                return fi.concat(gi);
            };

    @Test
    public void monoid_for_function_to_monoid() {
        // given
        final Function<Integer, String> toString = i -> Integer.toString(i);
        final Function<Integer, String> repeat = i -> StringUtils.repeat("x", i);
        // when
        final Function<Integer, String> computation1 = append.apply(toString, repeat);
        final Function<Integer, String> computation2 = Stream.of(toString, repeat).reduce(id, append);
        // then
        assertThat(
                computation1.apply(3),
                is(
                        "3xxx"
                )
        );
        // then
        assertThat(
                computation2.apply(3),
                is(
                        "3xxx"
                )
        );
    }
}
