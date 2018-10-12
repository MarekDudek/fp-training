package fp;

import org.junit.Test;

import java.util.List;

import static fp.CollectionsHelper.concat;
import static fp.MathUtil.even;
import static fp.MathUtil.odd;
import static java.util.Collections.nCopies;
import static java.util.Collections.shuffle;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.quicktheories.QuickTheory.qt;
import static org.quicktheories.generators.SourceDSL.integers;

public final class MoreMonoidsJ8Test {

    @Test
    public void exclusive_disjunction__property() {
        qt().
                withExamples(10_000).
                forAll(
                        integers().between(0, 100),
                        integers().between(0, 100)
                ).
                checkAssert(
                        (trues, falses) -> {
                            List<Boolean> booleans = concat(nCopies(trues, true), nCopies(falses, false));
                            shuffle(booleans);
                            Boolean onlyOne = booleans.stream().reduce(false, (p, q) -> p ^ q);
                            if (odd(trues))
                                assertTrue(onlyOne);
                            else
                                assertFalse(onlyOne);
                        }
                );
    }

    @Test
    public void exclusive_conjunction__property() {
        qt().
                withExamples(10_000).
                forAll(
                        integers().between(0, 100),
                        integers().between(0, 100)
                ).
                checkAssert(
                        (trues, falses) -> {
                            List<Boolean> booleans = concat(nCopies(trues, true), nCopies(falses, false));
                            shuffle(booleans);
                            Boolean allOrNone = booleans.stream().reduce(true, (p, q) -> p == q);
                            if (even(falses))
                                assertTrue(allOrNone);
                            else
                                assertFalse(allOrNone);
                        }
                );
    }
}
