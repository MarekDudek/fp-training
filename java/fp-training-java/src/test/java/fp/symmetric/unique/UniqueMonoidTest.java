package fp.symmetric.unique;

import fj.data.Stream;
import org.junit.Test;

import static fj.Ord.intOrd;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class UniqueMonoidTest {

    @Test
    public void empty_list_of_integers() {
        // when
        Unique<Integer> empty = UniqueMonoid.empty(intOrd);
        // then
        assertThat(empty, instanceOf(AllUnique.class));
    }

    @Test
    public void folding_unique() {
        // given
        Stream<Integer> numbers = Stream.arrayStream(1, 2, 3, 4, 5);
        // when
        Stream<Unique<Integer>> uniques = numbers.map(i -> UniqueMonoid.singleton(intOrd, i));
        Unique<Integer> result = uniques.foldLeft(UniqueMonoid::append, UniqueMonoid.empty(intOrd));
        // then
        assertThat(result, instanceOf(AllUnique.class));
    }

    @Test
    public void folding_duplicated() {
        // given
        Stream<Integer> numbers = Stream.arrayStream(5, 2, 3, 4, 5);
        // when
        Stream<Unique<Integer>> uniques = numbers.map(i -> UniqueMonoid.singleton(intOrd, i));
        Unique<Integer> result = uniques.foldLeft(UniqueMonoid::append, UniqueMonoid.empty(intOrd));
        // then
        assertThat(result, instanceOf(Duplicated.class));
        assertThat(result, is(new Duplicated<>(5)));
    }

    @Test
    public void is_unique__unique() {
        // given
        Stream<Integer> numbers = Stream.arrayStream(1, 2, 3, 4, 5);
        // when
        boolean unique = UniqueMonoid.allUnique(numbers, intOrd);
        // then
        assertThat(unique, is(true));
    }

    @Test
    public void is_unique__duplicated() {
        // given
        Stream<Integer> numbers = Stream.arrayStream(5, 2, 3, 4, 5);
        // when
        boolean unique = UniqueMonoid.allUnique(numbers, intOrd);
        // then
        assertThat(unique, is(false));
    }
}
