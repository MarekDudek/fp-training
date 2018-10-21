package fp;

import fj.F;
import fj.Monoid;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

import static fj.data.List.list;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class MonoidsOnFunctionsFjTest {


    @Test
    public void monoid_for_function_to_endomorphisms() {
        // given
        final F<String, String> a = s -> "Hello, ".concat(s);
        final F<String, String> b = s -> s.concat("!");

        // TODO: provide example
    }

    private final static Monoid<String> string_monoid = Monoid.stringMonoid;
    private final static Monoid<F<Integer, String>> function_to_string_monoid = Monoid.functionMonoid(string_monoid);

    @Test
    public void monoid_for_function_to_monoid() {
        // given
        final F<Integer, String> to_str = i -> Integer.toString(i);
        final F<Integer, String> repeat = i -> StringUtils.repeat("x", i);
        // when
        final F<Integer, String> computation1 = function_to_string_monoid.sum(to_str, repeat);
        final F<Integer, String> computation2 = function_to_string_monoid.sumRight(list(to_str, repeat));
        // then
        assertThat(
                computation1.f(3),
                is(
                        "3xxx"
                )
        );
        assertThat(
                computation2.f(3),
                is(
                        "3xxx"
                )
        );
    }
}
