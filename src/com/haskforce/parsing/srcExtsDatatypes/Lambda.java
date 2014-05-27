package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * Lambda l [Pat l] (Exp l)
 */
public class Lambda extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType[] pats;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "Lambda{" +
                "pats=" + Arrays.toString(pats) +
                ", exp=" + exp +
                '}';
    }
}
