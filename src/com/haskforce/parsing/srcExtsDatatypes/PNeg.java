package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PNeg l (Pat l)
 */
public class PNeg extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PNeg{" +
                "pat=" + pat +
                '}';
    }
}
