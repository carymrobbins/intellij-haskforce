package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * PList l [Pat l]
 */
public class PList extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType[] pats;

    @Override
    public String toString() {
        return "PList{" +
                "pats=" + Arrays.toString(pats) +
                '}';
    }
}
