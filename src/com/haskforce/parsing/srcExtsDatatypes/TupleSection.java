package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * TupleSection l Boxed [Maybe (Exp l)]  -- ^ tuple section expression, e.g. @(,,3)@
 */
public class TupleSection extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public BoxedTopType boxed;
    public ExpTopType[] expMaybes;

    @Override
    public String toString() {
        return "Tuple{" +
                "boxed=" + boxed +
                ", exps=" + Arrays.toString(expMaybes) +
                '}';
    }
}
