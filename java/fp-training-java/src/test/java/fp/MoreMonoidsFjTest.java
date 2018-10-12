package fp;

import fj.data.List;
import org.junit.Test;

import static fj.Monoid.exclusiveDisjunctionMonoid;
import static fj.data.List.list;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class MoreMonoidsFjTest {

    @Test
    public void exclusive_disjunction__odd_number_of_elements() {
        List<Boolean> odd = list(true, false, true, false, true);
        assertThat(
                exclusiveDisjunctionMonoid.sumRight(odd),
                is(
                        true
                ));
    }

    @Test
    public void exclusive_disjunction__even_number_of_elements() {
        List<Boolean> even = list(false, true, false, true, false);
        assertThat(
                exclusiveDisjunctionMonoid.sumRight(even),
                is(
                        false
                ));
    }
}
