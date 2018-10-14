package fp;

import lombok.Builder;
import org.junit.Test;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;

import static fp.CollectionsHelper.randomIntegers;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.iterate;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static pl.touk.throwing.ThrowingFunction.unchecked;

public final class ConcurrencyTest {

    static {
        System.err.println("constructing test data");
    }

    private static final int integers_count = 1_000_000;
    private static final List<Integer> integers = randomIntegers(integers_count);

    @Test
    public void single_pass() {
        System.err.println("starting single pass");
        Integer sum = integers.stream().reduce(0, (a, b) -> a + b);
        assertThat(sum, is(-1598933826));
    }

    @Test
    public void concurrent() {
        System.err.println("starting concurrent");
        // given
        ExecutorService service = Executors.newFixedThreadPool(8);
        CompletionService<Integer> completionService = new ExecutorCompletionService<>(service);
        // when
        final int taskCount = 4;
        final Collection<SumListOfIntegers> tasks = iterate(0, i -> i + 1).limit(taskCount).
                map(
                        i -> {
                            int fr = integers_count * i / taskCount;
                            int to = integers_count * (i + 1) / taskCount;
                            return new SumListOfIntegers(integers.subList(fr, to));
                        }
                ).
                collect(toList());
        tasks.forEach(completionService::submit);
        Integer sum = iterate(0, i -> i + 1).limit(taskCount).
                map(
                        unchecked(i -> completionService.take())
                ).
                map(
                        unchecked(Future::get)
                ).
                reduce(0, (a, b) -> a + b);
        // then
        System.err.println("sum: " + sum);
        assertThat(sum, is(-1598933826));
    }

    @Builder
    static final class SumListOfIntegers implements Callable<Integer> {

        public final List<Integer> list;

        @Override
        public Integer call() {
            return list.stream().reduce(0, (a, b) -> a + b);
        }
    }
}
