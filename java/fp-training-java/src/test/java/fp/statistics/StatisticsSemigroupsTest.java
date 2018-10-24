package fp.statistics;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.math3.stat.StatUtils;
import org.apache.commons.math3.util.Precision;
import org.junit.Test;

import java.util.Optional;
import java.util.stream.Stream;

import static fp.statistics.ArithmeticMean.arithmeticMean;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.quicktheories.QuickTheory.qt;
import static org.quicktheories.generators.SourceDSL.arrays;
import static org.quicktheories.generators.SourceDSL.doubles;

public final class StatisticsSemigroupsTest {

    @Test
    public void arithmetic_mean_as_semigroup() {
        // given
        ArithmeticMean a = arithmeticMean(1);
        ArithmeticMean b = arithmeticMean(3);
        ArithmeticMean c = arithmeticMean(6);
        // when
        Optional<ArithmeticMean> mean = Stream.of(a, b, c).reduce(ArithmeticMean::append);
        // then
        assertThat(mean.get().getArithmeticMean(), is(3.3333333333333335));
    }

    @Test
    public void arithmetic_mean_comparison_test() {
        qt().
                forAll(
                        arrays().ofClass(doubles().between(0, 100), Double.class).withLength(100)
                ).
                checkAssert(
                        ds -> {
                            double mean1 = Stream.of(ds).
                                    map(ArithmeticMean::arithmeticMean).
                                    reduce(ArithmeticMean::append).
                                    get().getArithmeticMean();
                            double mean2 = StatUtils.mean(ArrayUtils.toPrimitive(ds));
                            assertTrue(Precision.equals(mean1, mean2, 0.001));
                        }
                );
    }
}
