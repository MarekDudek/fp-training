package fp.symmetric.unique;

import fj.Ord;
import fj.data.Set;

import java.util.Iterator;

import static org.apache.commons.lang3.BooleanUtils.isFalse;

public enum UniqueMonoid {

    ;

    public static <A> Unique<A> singleton(Ord<A> ord, A a) {
        return new AllUnique<>(Set.single(ord, a));
    }

    public static <A> boolean allUnique(Iterable<A> elements, Ord<A> ord) {
        return allUnique(Set.empty(ord), elements.iterator());
    }

    private static <A> boolean allUnique(Set<A> soFar, Iterator<A> rest) {
        if (isFalse(rest.hasNext()))
            return true;
        A next = rest.next();
        if (soFar.member(next))
            return false;
        else
            return allUnique(soFar.insert(next), rest);
    }

    public static <A> Unique<A> empty(final Ord<A> ord) {
        return new AllUnique<>(Set.empty(ord));
    }

    public static <A> Unique<A> append(final Unique<A> x, final Unique<A> y) {
        return x.match(
                all ->
                        y.match(
                                too -> {
                                    Set<A> i = all.set.intersect(too.set);
                                    if (i.isEmpty())
                                        return new AllUnique<>(all.set.union(too.set));
                                    else
                                        return new Duplicated<>(i.iterator().next());
                                },
                                dup -> dup
                        ),
                dup -> dup
        );
    }

}
