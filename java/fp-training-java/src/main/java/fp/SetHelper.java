package fp;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import java.util.List;
import java.util.Set;

public final class SetHelper {

    public static <A> Set<A> set(A... as) {
        return Sets.newHashSet(as);
    }

    public static <A> List<A> list(A... as) {
        return Lists.newArrayList(as);
    }

    public static Set<Character> Omega = set(
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
            'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
            's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
    );
}
