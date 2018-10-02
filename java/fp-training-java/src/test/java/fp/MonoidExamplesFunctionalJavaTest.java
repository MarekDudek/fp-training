package fp;

import fj.Monoid;
import fj.data.List;
import fj.data.Set;
import org.junit.Test;

import static fj.Monoid.*;
import static fj.Ord.charOrd;
import static fj.data.List.list;
import static fj.data.Set.set;
import static org.hamcrest.Matchers.is;
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

    @Test
    public void union_of_sets() {
        List<Set<Character>> ss = list(set(charOrd, 'a', 'b', 'c'), set(charOrd, 'b', 'c', 'd'), set(charOrd, 'c', 'd', 'e'));
        Set<Character> s = setMonoid(charOrd).sumRight(ss);
        assertThat(s, is(set(charOrd, 'a', 'b', 'c', 'd', 'e')));
    }

    @Test
    public void intersection_of_sets() {
        // TODO: no such thing (yet) in FJ library
    }

    @Test
    public void appending_lists() {
        List<List<Integer>> ls = list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9));
        List<Integer> l = Monoid.<Integer>listMonoid().sumRight(ls);
        assertThat(l, is(list(1, 2, 3, 4, 5, 6, 7, 8, 9)));
    }

    @Test
    public void appending_strings() {
        List<String> ss = list("abc", "def", "ghi");
        String s = stringMonoid.sumRight(ss);
        assertThat(s, is("abcdefghi"));
    }
}
