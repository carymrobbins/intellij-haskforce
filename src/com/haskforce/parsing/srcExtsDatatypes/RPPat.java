package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  RPPat l (Pat l)
 */
public class RPPat extends RPatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;

    @Override
    public String toString() {
        return "RPPat{" +
                "pat=" + pat +
                '}';
    }
}
