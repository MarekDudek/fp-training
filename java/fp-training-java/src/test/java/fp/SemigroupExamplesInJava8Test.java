package fp;

import com.google.common.collect.Sets;
import org.apache.commons.collections.ListUtils;
import org.junit.Test;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import static fp.SetHelper.list;
import static fp.SetHelper.set;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class SemigroupExamplesInJava8Test {

    @Test
    public void minimum_of_numbers() {
        Stream<Integer> ns = Stream.of(1, 5, 2, 4, 3);
        Optional<Integer> minimum = ns.reduce((a, b) -> Math.min(a, b));
        assertThat(minimum, is(Optional.of(1)));
    }

    @Test
    public void maximum_of_numbers() {
        Stream<Integer> ns = Stream.of(1, 5, 2, 4, 3);
        Optional<Integer> maximum = ns.reduce((a, b) -> Math.max(a, b));
        assertThat(maximum, is(Optional.of(5)));
    }

    @Test
    public void addition_of_numbers() {
        Stream<Integer> ns = Stream.of(1, 2, 3, 4, 5);
        Optional<Integer> sum = ns.reduce((a, b) -> a + b);
        assertThat(sum, is(Optional.of(15)));
    }

    @Test
    public void multiplication_of_numbers() {
        Stream<Integer> ns = Stream.of(1, 2, 3, 4, 5);
        Optional<Integer> product = ns.reduce((a, b) -> a * b);
        assertThat(product, is(Optional.of(120)));
    }

    @Test
    public void logical_disjunction() {
        Stream<Boolean> bs = Stream.of(true, false, true, false, true);
        Optional<Boolean> any = bs.reduce((a, b) -> a || b);
        assertThat(any, is(Optional.of(true)));
    }

    @Test
    public void logical_conjunction() {
        Stream<Boolean> bs = Stream.of(true, false, true, false, true);
        Optional<Boolean> all = bs.reduce((a, b) -> a && b);
        assertThat(all, is(Optional.of(false)));
    }

    @Test
    public void intersection_of_sets() {
        Stream<Set<Character>> ss = Stream.of(set('a', 'b', 'c'), set('b', 'c', 'd'), set('c', 'd', 'e'));
        Optional<Set<Character>> intersection = ss.reduce((a, b) -> Sets.intersection(a, b));
        assertThat(intersection, is(Optional.of(set('c'))));
    }

    @Test
    public void appending_lists() {
        Stream<List<Integer>> ls = Stream.of(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9));
        Optional<List<Integer>> l = ls.reduce((a, b) -> ListUtils.union(a, b));
        assertThat(l, is(Optional.of(list(1, 2, 3, 4, 5, 6, 7, 8, 9))));
    }

    @Test
    public void appending_strings() {
        Stream<String> ss = Stream.of("abc", "def", "ghi");
        Optional<String> s = ss.reduce((a, b) -> a + b);
        assertThat(s, is(Optional.of("abcdefghi")));
    }
}
