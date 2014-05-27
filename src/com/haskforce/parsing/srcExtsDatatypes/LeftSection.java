package com.haskforce.parsing.srcExtsDatatypes;

/**
 * LeftSection l (Exp l) (QOp l)
 */
public class LeftSection extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;
    public QOpTopType qop;

    @Override
    public String toString() {
        return "LeftSection{" +
                "exp=" + exp +
                ", qop=" + qop +
                '}';
    }
}
