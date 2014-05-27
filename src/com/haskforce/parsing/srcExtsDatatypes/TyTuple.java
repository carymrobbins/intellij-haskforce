package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * TyTuple l Boxed [Type l]
 */
public class TyTuple extends TypeTopType {
    public SrcInfoSpan srcInfoSpan;
    public BoxedTopType boxed;
    public TypeTopType[] types;

    @Override
    public String toString() {
        return "TyTuple{" +
                "boxed=" + boxed +
                ", types=" + Arrays.toString(types) +
                '}';
    }
}
