package fp;

import java.util.Set;

import static com.google.common.collect.Sets.newHashSet;

public final class SetHelper {

    public static <A> Set<A> set(A... as) {
        return newHashSet(as);
    }
}
