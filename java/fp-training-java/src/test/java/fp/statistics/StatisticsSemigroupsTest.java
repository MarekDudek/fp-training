package fp.statistics;

import org.junit.Test;

import java.util.Optional;
import java.util.stream.Stream;

import static fp.statistics.Average.average;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class StatisticsSemigroupsTest {

    @Test
    public void average_as_monoid() {
        // given
        Average a = average(1);
        Average b = average(3);
        Average c = average(6);
        // when
        Optional<Average> average = Stream.of(a, b, c).reduce(Average::append);
        // then
        assertThat(average.get().getAverage(), is(3.3333333333333335));
    }
}
