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
        BinaryOperator<String> operator = (a, b) -> a.concat(b);
        String empty = "";
        // then
        assertThat(list.stream().reduce(empty, operator), is("abcdefghi"));
        // when
        BinaryOperator<String> dual = FunctionalUtilities.flip(operator);
        // then
        assertThat(list.stream().reduce(empty, dual), is("ghidefabc"));
    }
}
