package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * KindTuple l [Kind l]
 */
public class KindTuple extends KindTopType {
    public SrcInfoSpan srcInfoSpan;
    public KindTopType[] kinds;

    @Override
    public String toString() {
        return "KindTuple{" +
                "kinds=" + Arrays.toString(kinds) +
                '}';
    }
}
