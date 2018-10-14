package fp;

import org.junit.Test;

import java.util.List;

import static fp.CollectionsHelper.randomCharacters;
import static fp.CollectionsHelper.randomIntegers;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class ConcurrentConcatenationTest {

    static {
        System.err.println("constructing test data");
    }

    private static final int integers_count = 1_000_000;
    private static final List<Integer> integers = randomIntegers(integers_count);

    private static final List<String> characters = randomCharacters(100_000);

    @Test
    public void serial_sum() {
        Integer sum1 = integers.stream().reduce(0, (a, b) -> a + b);
        Integer sum2 = integers.stream().reduce(0, (a, b) -> b + a);
        assertThat(sum1, is(sum2));
    }

    @Test
    public void parallel_sum() {
        Integer sum1 = integers.parallelStream().reduce(0, (a, b) -> a + b);
        Integer sum2 = integers.parallelStream().reduce(0, (a, b) -> b + a);
        assertThat(sum1, is(sum2));
    }

    @Test
    public void serial_concatenation() {
        String serial = characters.stream().reduce("", String::concat);
        String parallel = characters.parallelStream().reduce("", String::concat);
        assertThat(serial, is(parallel));
    }
}
