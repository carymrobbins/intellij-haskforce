package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PViewPat l (Exp l) (Pat l)
 */
public class PViewPat extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PViewPat{" +
                "exp=" + exp +
                ", pat=" + pat +
                '}';
    }
}
