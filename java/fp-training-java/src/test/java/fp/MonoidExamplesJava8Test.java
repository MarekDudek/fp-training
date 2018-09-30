package fp;

import com.google.common.collect.Sets;
import org.apache.commons.collections.ListUtils;
import org.junit.Test;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import static fp.CollectionsHelper.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public final class MonoidExamplesJava8Test {

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
        Set<Character> union = ss.reduce(Collections.emptySet(), (a, b) -> Sets.union(a, b));
        assertThat(union, is(set('a', 'b', 'c', 'd', 'e')));
    }

    @Test
    public void intersection_of_sets() {
        Stream<Set<Character>> ss = Stream.of(set('a', 'b', 'c'), set('b', 'c', 'd'), set('c', 'd', 'e'));
        Set<Character> intersection = ss.reduce(Omega, (a, b) -> Sets.intersection(a, b));
        assertThat(intersection, is(set('c')));
    }

    @Test
    public void appending_lists() {
        Stream<List<Integer>> ls = Stream.of(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9));
        List<Integer> l = ls.reduce(Collections.emptyList(), (a, b) -> ListUtils.union(a, b));
        assertThat(l, is(list(1, 2, 3, 4, 5, 6, 7, 8, 9)));
    }

    @Test
    public void appending_strings() {
        Stream<String> ss = Stream.of("abc", "def", "ghi");
        String s = ss.reduce("", (a, b) -> a + b);
        assertThat(s, is("abcdefghi"));
    }
}
