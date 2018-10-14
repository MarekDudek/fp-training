package fp;

import lombok.Builder;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.List;
import java.util.concurrent.*;
import java.util.stream.Stream;

import static fp.CollectionsHelper.randomIntegers;
import static java.util.stream.Stream.iterate;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static pl.touk.throwing.ThrowingFunction.unchecked;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class ConcurrencyTest {

    private static final int integers_count = 100_000_000;
    private static final List<Integer> integers = randomIntegers(integers_count);

    @Test
    public void _1_single_pass() {
        singlePass();
    }

    @Test
    public void _2_concurrent() {
        concurrent();
    }

    @Test
    public void _3_single_pass() {
        singlePass();
    }

    @Test
    public void _4_concurrent() {
        concurrent();
    }

    private void concurrent() {
        // given
        final ExecutorService executor = Executors.newFixedThreadPool(7);
        final CompletionService<Integer> completion = new ExecutorCompletionService<>(executor);
        // when
        final int count = 7;
        final Stream<SumListOfIntegers> tasks =
                iterate(0, i -> i + 1).limit(count).map(
                        i -> {
                            int from = integers_count * i / count;
                            int to = integers_count * (i + 1) / count;
                            return new SumListOfIntegers(integers.subList(from, to));
                        }
                );
        tasks.forEach(completion::submit);
        final Integer sum =
                iterate(0, i -> i + 1).limit(count).map(
                        unchecked(i -> completion.take())
                ).map(
                        unchecked(Future::get)
                ).reduce(0, (a, b) -> a + b);
        // then
        assertThat(sum, is(107565984));
    }

    private void singlePass() {
        // when
        final Integer sum = integers.stream().reduce(0, (a, b) -> a + b);
        // then
        assertThat(sum, is(107565984));
    }

    @Builder
    static final class SumListOfIntegers implements Callable<Integer> {

        public final List<Integer> list;

        @Override
        public Integer call() {
            return list.parallelStream().reduce(0, (a, b) -> a + b);
        }
    }
}
