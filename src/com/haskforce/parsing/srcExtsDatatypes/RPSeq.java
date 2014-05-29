package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * RPSeq l [RPat l]
 */
public class RPSeq extends RPatTopType {
    public SrcInfoSpan srcInfoSpan;
    public RPatTopType[] rpats;

    @Override
    public String toString() {
        return "RPSeq{" +
                "rpats=" + Arrays.toString(rpats) +
                '}';
    }
}
