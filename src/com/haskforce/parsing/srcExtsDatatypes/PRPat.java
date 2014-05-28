package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PRPat l [RPat l]
 */
public class PRPat extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public RPatTopType[] rpats;

    @Override
    public String toString() {
        return "PRPat{" +
                "rpats=" + Arrays.toString(rpats) +
                '}';
    }
}
