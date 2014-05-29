package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RPParen l (RPat l)
 */
public class RPParen extends RPatTopType {
    public SrcInfoSpan srcInfoSpan;
    public RPatTopType rpat;

    @Override
    public String toString() {
        return "RPParen{" +
                "rpat=" + rpat +
                '}';
    }
}
