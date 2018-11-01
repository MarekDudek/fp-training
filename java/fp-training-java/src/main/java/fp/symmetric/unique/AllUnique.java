package fp.symmetric.unique;

import fj.data.Set;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.ToString;

@EqualsAndHashCode
@AllArgsConstructor
@ToString
public final class AllUnique<A> implements Unique<A> {
    public final Set<A> set;
}
