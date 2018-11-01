package fp.symmetric.unique;

import java.util.function.Function;

interface Unique<A> {

    default <B> B match
            (
                    Function<AllUnique<A>, B> allUnique,
                    Function<Duplicated<A>, B> duplicated
            ) {

        if (this instanceof AllUnique)
            return allUnique.apply((AllUnique<A>) this);
        if (this instanceof Duplicated)
            return duplicated.apply((Duplicated<A>) this);

        return null;
    }
}
