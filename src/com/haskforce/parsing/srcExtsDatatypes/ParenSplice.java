package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ParenSplice l (Exp l)
 */
public class ParenSplice extends SpliceTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "ParenSplice{" +
                "exp=" + exp +
                '}';
    }
}
