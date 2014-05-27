package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * Tuple l Boxed [Exp l]
 */
public class Tuple extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public BoxedTopType boxed;
    public ExpTopType[] exps;

    @Override
    public String toString() {
        return "Tuple{" +
                "boxed=" + boxed +
                ", exps=" + Arrays.toString(exps) +
                '}';
    }
}
