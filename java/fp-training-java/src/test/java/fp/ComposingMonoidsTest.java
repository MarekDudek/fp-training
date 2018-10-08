package fp;

import fj.Monoid;
import fj.P2;
import fj.data.List;
import fj.data.Option;
import org.junit.Test;

import static fj.Monoid.intMaxMonoid;
import static fj.Monoid.intMinMonoid;
import static fj.P.p;
import static fj.data.List.list;
import static fj.data.Option.none;
import static fj.data.Option.some;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class ComposingMonoidsTest {


    private static final Monoid<
            P2<
                    Integer,
                    Integer
                    >
            > min_max = intMinMonoid.compose(intMaxMonoid);

    @Test
    public void composing_sum_and_product_on_two_arguments() {
        assertThat(
                min_max.sum(p(2, 1), p(9, 7)),
                is(
                        p(2, 7)
                ));
    }

    @Test
    public void composing_min_and_max_on_many_arguments() {
        List<Integer> l = list(5, 4, 6, 3, 7, 2, 8, 1, 9);
        List<P2<Integer, Integer>> ll = l.zip(l);
        assertThat(
                min_max.sumRight(ll),
                is(
                        p(1, 9)
                ));
    }

    private static final Monoid<
            Option<String>
            > frst_string = Monoid.firstOptionMonoid();
    private static final Monoid<
            Option<String>
            > last_string = Monoid.lastOptionMonoid();

    private static final Monoid<
            P2<
                    Option<String>,
                    Option<String>
                    >
            > first_last_string =
            frst_string.compose(last_string);

    @Test
    public void combining_two_option_wrappers() {
        P2<Option<String>, Option<String>> p = p(some("start"), some("end"));
        assertThat(
                first_last_string.sum(p, p),
                is(
                        p
                ));

    }

    private static final List<Option<String>> list_with_gaps =
            list(
                    none(), some("This"), none(), some("list"), none(),
                    some("has"), none(), some("some"), none(), some("gaps"), none()
            );

    @Test
    public void combining_multiple_option_wrappers() {
        assertThat(
                first_last_string.sumRight(list_with_gaps.zip(list_with_gaps)),
                is(
                        p(some("This"), some("gaps"))
                ));
    }
}
