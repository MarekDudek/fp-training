package fp;

import org.junit.Test;

import java.util.Optional;
import java.util.stream.Stream;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class SemigroupExamples {

    @Test
    public void minimum_of_numbers() {
        Stream<Integer> ns = Stream.of(1, 5, 2, 4, 3);
        Optional<Integer> minimum = ns.reduce((a, b) -> Math.min(a, b));
        assertThat(minimum, is(Optional.of(1)));
    }

    @Test
    public void maximum_of_numbers() {
        Stream<Integer> ns = Stream.of(1, 5, 2, 4, 3);
        Optional<Integer> maximum = ns.reduce((a, b) -> Math.max(a, b));
        assertThat(maximum, is(Optional.of(5)));
    }
}
