package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PIrrPat l (Pat l)
 */
public class PIrrPat extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PIrrPat{" +
                "pat=" + pat +
                '}';
    }
}
