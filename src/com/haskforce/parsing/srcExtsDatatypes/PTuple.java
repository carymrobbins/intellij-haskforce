package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PTuple l Boxed [Pat l]
 */
public class PTuple extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public BoxedTopType boxed;
    public PatTopType[] pats;

    @Override
    public String toString() {
        return "PTuple{" +
                "boxed=" + boxed +
                ", pats=" + Arrays.toString(pats) +
                '}';
    }
}
