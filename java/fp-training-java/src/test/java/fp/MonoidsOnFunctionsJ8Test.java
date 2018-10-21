package fp;

import org.apache.commons.collections.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

import java.util.Collections;
import java.util.List;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.function.UnaryOperator.identity;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class MonoidsOnFunctionsJ8Test {

    @Test
    public void monoid_for_endomorphisms() {
        // given
        final UnaryOperator<String> a = s -> "Hello, ".concat(s);
        final UnaryOperator<String> b = s -> s.concat("!");
        // when
        final UnaryOperator<String> computation1 = UnaryOperatorOps.andThen(a, b);
        final UnaryOperator<String> computation2 = UnaryOperatorOps.compose(b, a);
        final UnaryOperator<String> computation3 = Stream.of(a, b).reduce(identity(), UnaryOperatorOps::andThen);
        // then
        assertThat(
                computation1.apply("World"),
                is(
                        "Hello, World!"
                )
        );
        assertThat(
                computation2.apply("World"),
                is(
                        "Hello, World!"
                )
        );
        assertThat(
                computation3.apply("World"),
                is(
                        "Hello, World!"
                )
        );
    }

    private static final String string__id = "";
    private static final BinaryOperator<String> string__append = String::concat;

    private static final Function<Integer, String> function_to_string__id = i -> string__id;
    private static final BinaryOperator<
            Function<Integer, String>
            > function_to_string__append =
            (f, g) ->
                    i ->
                            string__append.apply(f.apply(i), g.apply(i));

    @Test
    public void monoid_for_function_to_monoid() {
        // given
        final Function<Integer, String> to_str = i -> Integer.toString(i);
        final Function<Integer, String> repeat = i -> StringUtils.repeat("x", i);
        // when
        final Function<Integer, String> computation1 =
                function_to_string__append.apply(to_str, repeat);
        final Function<Integer, String> computation2 =
                Stream.of(to_str, repeat).reduce(function_to_string__id, function_to_string__append);
        // then
        assertThat(
                computation1.apply(3),
                is(
                        "3xxx"
                )
        );
        // then
        assertThat(
                computation2.apply(3),
                is(
                        "3xxx"
                )
        );
    }

    private static final List<String> list__id = Collections.emptyList();
    private static final BinaryOperator<List<String>> list__append = ListUtils::union;

    private static final Function<Integer, List<String>> function_to_list__id = i -> list__id;
    private static final BinaryOperator<Function<Integer, List<String>>> function_to_list__append =
            (f, g) ->
                    i ->
                            list__append.apply(f.apply(i), g.apply(i));

    @Test
    public void another_monoid_for_function_to_monoid() {
        // given
        final Function<Integer, List<String>> f =
                i ->
                        Stream.iterate(1, j -> j + 1).limit(i).
                                map(n -> Integer.toString(n)).
                                collect(toList());
        final Function<Integer, List<String>> g =
                i ->
                        Stream.iterate(1, j -> j + 1).limit(i).
                                map(n -> Collections.nCopies(n, "x").stream().collect(joining(""))).
                                collect(toList());
        // when
        final Function<Integer, List<String>> h =
                Stream.of(f, g).reduce(function_to_list__id, function_to_list__append);
        // then
        assertThat(
                h.apply(3),
                is(
                        asList(
                                "1", "2", "3", "x", "xx", "xxx"
                        )
                )
        );

    }
}
