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
}
