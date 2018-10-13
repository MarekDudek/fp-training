package fp;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import org.apache.commons.collections.ListUtils;

import java.util.List;
import java.util.Set;
import java.util.SplittableRandom;

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

    private static final SplittableRandom RANDOM = new SplittableRandom();

    public static List<Integer> randomListOfIntegersOfLength(final Integer length) {
        List<Integer> list = Lists.newArrayListWithExpectedSize(length);
        for (int i = 0; i < length; i++)
            list.add(RANDOM.nextInt());
        return list;
    }
}
