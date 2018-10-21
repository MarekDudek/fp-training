package fp.statistics;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.math3.stat.StatUtils;
import org.apache.commons.math3.util.Precision;
import org.junit.Test;

import java.util.Optional;
import java.util.stream.Stream;

import static fp.statistics.Average.average;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.quicktheories.QuickTheory.qt;
import static org.quicktheories.generators.SourceDSL.arrays;
import static org.quicktheories.generators.SourceDSL.doubles;

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

    @Test
    public void arithmetic_mean_comparison_test() {
        qt().
                forAll(
                        arrays().ofClass(doubles().between(0, 100), Double.class).withLength(100)
                ).
                checkAssert(
                        ds -> {
                            double avg1 = Stream.of(ds).
                                    map(Average::average).
                                    reduce(Average::append).
                                    get().getAverage();
                            double avg2 = StatUtils.mean(ArrayUtils.toPrimitive(ds));
                            assertTrue(Precision.equals(avg1, avg2, 0.001));
                        }
                );
    }
}
