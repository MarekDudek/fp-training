package fp;

import fj.data.NonEmptyList;
import org.junit.Test;

import static fj.Semigroup.*;
import static fj.data.NonEmptyList.nel;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class SemigroupExamplesFjTest {

    @Test
    public void minimum_of_numbers() {
        NonEmptyList<Integer> ns = nel(5, 1, 4, 2, 3);
        Integer minimum = intMinimumSemigroup.sumNel(ns);
        assertThat(minimum, is(1));
    }

    @Test
    public void maximum_of_numbers() {
        NonEmptyList<Integer> ns = nel(5, 1, 4, 2, 3);
        Integer maximum = intMaximumSemigroup.sumNel(ns);
        assertThat(maximum, is(5));
    }

    @Test
    public void addition_of_numbers() {
        NonEmptyList<Integer> ns = nel(1, 2, 3, 4, 5);
        Integer sum = intAdditionSemigroup.sumNel(ns);
        assertThat(sum, is(15));
    }

    @Test
    public void multiplication_of_numbers() {
        NonEmptyList<Integer> ns = nel(1, 2, 3, 4, 5);
        Integer product = intMultiplicationSemigroup.sumNel(ns);
        assertThat(product, is(120));
    }

    @Test
    public void logical_disjunction() {
        NonEmptyList<Boolean> bs = nel(true, false, true, false, true);
        Boolean any = disjunctionSemigroup.sumNel(bs);
        assertThat(any, is(true));
    }

    @Test
    public void logical_conjunction() {
        NonEmptyList<Boolean> bs = nel(true, false, true, false, true);
        Boolean all = conjunctionSemigroup.sumNel(bs);
        assertThat(all, is(false));
    }
}
