package fp;

import org.junit.Test;

import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class ConcurrentConcatenationTest {

    private static List<Integer> list = CollectionsHelper.randomListOfIntegersOfLength(10_000_000);

    @Test
    public void parallel() {
        Integer sum1 = list.parallelStream().reduce(0, (a, b) -> a + b);
        Integer sum2 = list.parallelStream().reduce(0, (a, b) -> b + a);
        assertThat(sum1, is(sum2));
    }

    @Test
    public void serial() {
        Integer sum1 = list.stream().reduce(0, (a, b) -> a + b);
        Integer sum2 = list.stream().reduce(0, (a, b) -> b + a);
        assertThat(sum1, is(sum2));
    }
}
