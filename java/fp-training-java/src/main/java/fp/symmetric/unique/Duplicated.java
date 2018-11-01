package fp.symmetric.unique;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.ToString;

@EqualsAndHashCode
@AllArgsConstructor
@ToString
public final class Duplicated<A> implements Unique<A> {
    public final A example;
}
