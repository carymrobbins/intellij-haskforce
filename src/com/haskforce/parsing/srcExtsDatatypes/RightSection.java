package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RightSection l (QOp l) (Exp l)
 */
public class RightSection extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public QOpTopType qop;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "RightSection{" +
                "qop=" + qop +
                ", exp=" + exp +
                '}';
    }
}
