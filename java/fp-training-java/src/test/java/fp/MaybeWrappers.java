package fp;

import fj.Monoid;
import fj.data.Option;
import org.junit.Test;

import static fj.Semigroup.intMinimumSemigroup;
import static fj.data.List.list;
import static fj.data.Option.none;
import static fj.data.Option.some;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class MaybeWrappers {

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
}
