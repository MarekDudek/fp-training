package fp;

import fj.Semigroup;
import fj.data.List;
import fj.data.NonEmptyList;
import fj.data.Set;
import org.junit.Test;

import static fj.Ord.charOrd;
import static fj.Semigroup.*;
import static fj.data.List.list;
import static fj.data.NonEmptyList.nel;
import static fj.data.Set.set;
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
    public void addition_of_two_numbers() {
        Integer sum = intAdditionSemigroup.sum(2, 5);
        assertThat(sum, is(7));
    }

    @Test
    public void addition_of_numbers() {
        NonEmptyList<Integer> ns = nel(1, 2, 3, 4, 5);
        Integer sum = intAdditionSemigroup.sumNel(ns);
        assertThat(sum, is(15));
    }

    @Test
    public void multiplication_of_two_numbers() {
        Integer product = intMultiplicationSemigroup.sum(2, 5);
        assertThat(product, is(10));
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

    @Test
    public void union_of_sets() {
        NonEmptyList<Set<Character>> ss = nel(set(charOrd, 'a', 'b', 'c'), set(charOrd, 'b', 'c', 'd'), set(charOrd, 'c', 'd', 'e'));
        Set<Character> s = Semigroup.<Character>setSemigroup().sumNel(ss);
        assertThat(s, is(set(charOrd, 'a', 'b', 'c', 'd', 'e')));
    }

    @Test
    public void intersection_of_sets() {
        // TODO: no such thing (yet) in FJ library
    }

    @Test
    public void appending_lists() {
        NonEmptyList<List<Integer>> ls = nel(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9));
        List<Integer> l = Semigroup.<Integer>listSemigroup().sumNel(ls);
        assertThat(l, is(list(1, 2, 3, 4, 5, 6, 7, 8, 9)));
    }

    @Test
    public void string_concatenation() {
        NonEmptyList<String> ss = nel("abc", "def", "ghi");
        String s = stringSemigroup.sumNel(ss);
        assertThat(s, is("abcdefghi"));
    }
}
