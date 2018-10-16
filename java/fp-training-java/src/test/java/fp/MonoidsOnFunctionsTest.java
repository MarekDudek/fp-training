package fp;

import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class MonoidsOnFunctionsTest {

    @Test
    public void monoid_for_endomorphisms() {
        // given
        final Endo<String> h = s -> "Hello, ".concat(s);
        final Endo<String> w = s -> s.concat("!");
        // when
        final Endo<String> computation1 = h.andThen(w);
        final Endo<String> computation2 = w.compose(h);
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
