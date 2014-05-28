package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PBangPat l (Pat l)
 */
public class PBangPat extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PBangPat{" +
                "pat=" + pat +
                '}';
    }
}
