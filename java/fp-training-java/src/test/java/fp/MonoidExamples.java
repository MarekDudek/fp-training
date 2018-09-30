package fp;

import com.google.common.collect.Sets;
import org.junit.Test;

import java.util.Set;
import java.util.stream.Stream;

import static fp.SetHelper.set;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public final class MonoidExamples {

    @Test
    public void addition_of_numbers() {
        Stream<Integer> ns = Stream.of(1, 2, 3, 4, 5);
        Integer sum = ns.reduce(0, (a, b) -> a + b);
        assertThat(sum, is(15));
    }

    @Test
    public void multiplication_of_numbers() {
        Stream<Integer> ns = Stream.of(1, 2, 3, 4, 5);
        Integer product = ns.reduce(1, (a, b) -> a * b);
        assertThat(product, is(120));
    }

    @Test
    public void logical_disjunction() {
        Stream<Boolean> bs = Stream.of(true, false, true, false, true);
        Boolean any = bs.reduce(false, (a, b) -> a || b);
        assertThat(any, is(true));
    }

    @Test
    public void logical_conjunction() {
        Stream<Boolean> bs = Stream.of(true, false, true, false, true);
        Boolean all = bs.reduce(true, (a, b) -> a && b);
        assertThat(all, is(false));
    }

    @Test
    public void union_of_sets() {
        Stream<Set<Character>> ss = Stream.of(set('a', 'b', 'c'), set('b', 'c', 'd'), set('c', 'd', 'e'));
        Set<Character> union = ss.reduce(set(), (a, b) -> Sets.union(a, b));
        assertThat(union, is(set('a', 'b', 'c', 'd', 'e')));
    }
}
