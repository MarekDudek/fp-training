package fp.symmetric.unique;

import fj.Ord;
import fj.data.List;
import fj.data.Set;

public enum UniqueMonoid {

    ;

    public static <A> Unique<A> singleton(Ord<A> ord, A a) {
        return new AllUnique<>(Set.single(ord, a));
    }

    public static <A> boolean allUnique(Iterable<A> elements, Ord<A> ord) {
        return allUnique(Set.empty(ord), List.iterableList(elements));
    }

    private static <A> boolean allUnique(Set<A> soFar, List<A> rest) {
        if (rest.isEmpty())
            return true;
        if (soFar.member(rest.head()))
            return false;
        else
            return allUnique(soFar.insert(rest.head()), rest.tail());
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
