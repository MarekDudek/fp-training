package fp;

import fj.Monoid;
import fj.Semigroup;
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
}
