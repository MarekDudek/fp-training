package fp;

import com.google.common.collect.ImmutableList;
import fj.Monoid;
import fj.data.List;
import org.junit.Test;

import java.util.function.BinaryOperator;

import static com.google.common.collect.ImmutableList.of;
import static fj.Monoid.stringMonoid;
import static fj.data.List.list;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public final class MoreFunTest {

    @Test
    public void dual_in_FJ() {
        // given
        List<String> ss = list("abc", "def", "ghi");
        Monoid<String> monoid = stringMonoid;
        // when
        assertThat(monoid.sumRight(ss), is("abcdefghi"));
        // when
        Monoid<String> dual = monoid.dual();
        // then
        assertThat(dual.sumRight(ss), is("ghidefabc"));
    }

    @Test
    public void dual_in_J8() {
        // given
        ImmutableList<String> list = of("abc", "def", "ghi");
        BinaryOperator<String> monoid = (a, b) -> a.concat(b);
        // then
        assertThat(list.stream().reduce("", monoid), is("abcdefghi"));
        // when
        BinaryOperator<String> dual = FunctionalUtilities.flip(monoid);
        // then
        assertThat(list.stream().reduce("", dual), is("ghidefabc"));
    }
}
