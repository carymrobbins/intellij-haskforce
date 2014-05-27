package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * MultiIf l [IfAlt l]
 */
public class MultiIf extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public IfAlt[] alts;

    @Override
    public String toString() {
        return "MultiIf{" +
                "alts=" + Arrays.toString(alts) +
                '}';
    }
}
