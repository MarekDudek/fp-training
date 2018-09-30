package fp;

import fj.data.List;
import org.junit.Test;

import static fj.Monoid.*;
import static fj.data.List.list;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public final class MonoidExamplesFunctionalJavaTest {

    @Test
    public void addition_of_numbers() {
        List<Integer> ns = list(1, 2, 3, 4, 5);
        Integer sum = intAdditionMonoid.sumRight(ns);
        assertThat(sum, is(15));
    }

    @Test
    public void multiplication_of_numbers() {
        List<Integer> ns = list(1, 2, 3, 4, 5);
        Integer product = intMultiplicationMonoid.sumRight(ns);
        assertThat(product, is(120));
    }

    @Test
    public void logical_disjunction() {
        List<Boolean> bs = list(true, false, true, false, true);
        Boolean any = disjunctionMonoid.sumRight(bs);
        assertThat(any, is(true));
    }

    @Test
    public void logical_conjunction() {
        List<Boolean> bs = list(true, false, true, false, true);
        Boolean all = conjunctionMonoid.sumRight(bs);
        assertThat(all, is(false));
    }
}
