package fp;

import org.junit.Test;

import java.util.function.UnaryOperator;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class MonoidsOnFunctionsTest {

    @Test
    public void monoid_for_endomorphisms() {
        // given
        final UnaryOperator<String> h = s -> "Hello, ".concat(s);
        final UnaryOperator<String> w = s -> s.concat("!");
        // when
        final UnaryOperator<String> computation1 = UnaryOperatorOps.andThen(h, w);
        final UnaryOperator<String> computation2 = UnaryOperatorOps.compose(w, h);
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
    }
}
