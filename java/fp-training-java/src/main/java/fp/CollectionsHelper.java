package fp;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import org.apache.commons.collections.ListUtils;

import java.util.List;
import java.util.Random;
import java.util.Set;

import static java.util.stream.Stream.iterate;

public final class CollectionsHelper {

    @SafeVarargs
    public static <A> Set<A> set(A... as) {
        return Sets.newHashSet(as);
    }

    @SafeVarargs
    public static <A> List<A> list(A... as) {
        return Lists.newArrayList(as);
    }

    @SuppressWarnings("unchecked")
    public static <A> List<A> concat(List<A> x, List<A> y) {
        return ListUtils.union(x, y);
    }

    public static Set<Character> Omega = set(
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
            'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
            's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
    );

    private static final Random RANDOM = new Random(0);

    public static List<Integer> randomIntegers(final Integer length) {
        final Random generator = new Random(0);
        final List<Integer> result = Lists.newArrayListWithExpectedSize(length);
        iterate(0, i -> i + 1).limit(length).forEach(
                i -> result.add(generator.nextInt())
        );
        return result;
    }

    private static final int LowestLetter = (int) 'a';
    private static final int HighestLetter = (int) 'z';

    public static List<String> randomCharacters(final int length) {
        final Random generator = new Random(0);
        final List<String> list = Lists.newArrayListWithExpectedSize(length);
        iterate(0, i -> i + 1).limit(length).parallel().forEach(
                i -> {
                    int r = LowestLetter + generator.nextInt(HighestLetter - LowestLetter);
                    list.add(Character.toString((char) r));
                }
        );
        return list;
    }
}
