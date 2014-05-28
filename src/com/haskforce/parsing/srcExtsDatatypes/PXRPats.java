package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PXRPats  l [RPat l]
 */
public class PXRPats extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public RPatTopType[] rPats;

    @Override
    public String toString() {
        return "PXRPats{" +
                "rPats=" + Arrays.toString(rPats) +
                '}';
    }
}
