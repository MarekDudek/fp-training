package fp;

import fj.Monoid;
import fj.Semigroup;
import fj.data.List;
import fj.data.Option;
import org.junit.Test;

import static fj.Semigroup.intMinimumSemigroup;
import static fj.data.List.list;
import static fj.data.Option.none;
import static fj.data.Option.some;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class MaybeWrappersTest {

    private static final Monoid<Option<Integer>> MinMonoid = intMinimumSemigroup.lift();

    @Test
    public void minimum_of_two_numbers() {
        Option<Integer> min = MinMonoid.sum(some(2), some(5));
        assertThat(min, is(some(2)));
    }

    @Test
    public void minimum_of_one_and_empty() {
        Option<Integer> min = MinMonoid.sum(some(2), none());
        assertThat(min, is(some(2)));
    }

    @Test
    public void minimum_of_two_empties() {
        Option<Integer> min = MinMonoid.sum(none(), none());
        assertThat(min, is(none()));
    }

    @Test
    public void minimum_of_many_empties() {
        Option<Integer> min = MinMonoid.sumRight(list(none(), none(), none()));
        assertThat(min, is(none()));
    }

    private static final Monoid<Option<Integer>> SumMonoidWrapper = Semigroup.intAdditionSemigroup.lift();

    @Test
    public void sum_of_two_numbers() {
        Option<Integer> sum = SumMonoidWrapper.sum(some(3), some(5));
        assertThat(sum, is(some(8)));
    }

    private static final Monoid<Option<Integer>> ProductMonoidWrapper = Semigroup.intMultiplicationSemigroup.lift();

    @Test
    public void product_of_two_numbers() {
        Option<Integer> product = ProductMonoidWrapper.sum(some(3), some(5));
        assertThat(product, is(some(15)));
    }

    private static final Monoid<Option<Integer>> firstInteger = Monoid.firstOptionMonoid();

    @Test
    public void first_of_two_value() {
        assertThat(
                firstInteger.sum(some(3), some(5)), is(some(3))
        );
        assertThat(
                firstInteger.sum(some(3), none()), is(some(3))
        );
        assertThat(
                firstInteger.sum(none(), some(5)), is(some(5))
        );
        assertThat(
                firstInteger.sum(none(), none()), is(none())
        );
    }

    private static final Monoid<Option<String>> firstString = Monoid.<String>firstOptionMonoid();

    @Test
    public void first_of_many_values() {
        List<Option<String>> l =
                list(none(), some("This"), none(), some("l"), none(), some("has"), none(), some("gaps"), none());
        assertThat(
                firstString.sumRight(l), is(some("This"))
        );
        assertThat(
                firstString.sumLeft(l), is(some("This"))
        );
    }

    private static final Monoid<Option<Integer>> lastInteger = Monoid.lastOptionMonoid();

    @Test
    public void last_of_two_value() {
        assertThat(
                lastInteger.sum(some(3), some(5)), is(some(5))
        );
        assertThat(
                lastInteger.sum(some(3), none()), is(some(3))
        );
        assertThat(
                lastInteger.sum(none(), some(5)), is(some(5))
        );
        assertThat(
                lastInteger.sum(none(), none()), is(none())
        );
    }

    private static final Monoid<Option<String>> lastString = Monoid.lastOptionMonoid();

    @Test
    public void last_of_many_values() {
        List<Option<String>> l =
                list(none(), some("This"), none(), some("list"), none(), some("has"), none(), some("gaps"), none());
        assertThat(
                lastString.sumRight(l), is(some("gaps"))
        );
        assertThat(
                lastString.sumLeft(l), is(some("gaps"))
        );
    }

}
