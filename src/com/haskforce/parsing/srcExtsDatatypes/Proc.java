package com.haskforce.parsing.srcExtsDatatypes;

/**
 * Proc            l (Pat l) (Exp l)
 */
public class Proc extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "Proc{" +
                "pat=" + pat +
                ", exp=" + exp +
                '}';
    }
}
