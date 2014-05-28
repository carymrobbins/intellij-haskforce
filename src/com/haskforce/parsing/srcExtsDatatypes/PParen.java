package com.haskforce.parsing.srcExtsDatatypes;

/**
 * PParen l (Pat l)
 */
public class PParen extends PatTopType {
    public SrcInfoSpan srcInfoSpan;
    public PatTopType pat;

    @Override
    public String toString() {
        return "PParen{" +
                "pat=" + pat +
                '}';
    }
}
