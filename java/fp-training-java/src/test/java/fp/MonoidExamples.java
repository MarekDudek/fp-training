package fp;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public final class MonoidExamples {

    @Test
    public void addition_of_numbers() {
        // given
        final List<Integer> ns = Arrays.asList(1, 2, 3, 4, 5);
        // when
        final Integer sum = ns.stream().reduce(0, (a, b) -> a + b);
        // then
        assertThat(sum, is(15));
    }

    @Test
    public void multiplication_of_numbers() {
        // given
        final List<Integer> ns = Arrays.asList(1, 2, 3, 4, 5);
        // when
        final Integer product = ns.stream().reduce(1, (a, b) -> a * b);
        // then
        assertThat(product, is(120));
    }

    @Test
    public void logical_disjunction() {
        // given
        final List<Boolean> bs = Arrays.asList(true, false, true, false, true);
        // when
        final Boolean any = bs.stream().reduce(false, (a, b) -> a || b);
        // then
        assertThat(any, is(true));
    }

    @Test
    public void logical_conjunction() {
        // given
        final List<Boolean> bs = Arrays.asList(true, false, true, false, true);
        // when
        final Boolean all = bs.stream().reduce(true, (a, b) -> a && b);
        // then
        assertThat(all, is(false));
    }
}
