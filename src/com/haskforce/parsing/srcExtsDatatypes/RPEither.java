package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RPEither l (RPat l) (RPat l)
 */
public class RPEither extends RPatTopType {
    public SrcInfoSpan srcInfoSpan;
    public RPatTopType r1;
    public RPatTopType r2;

    @Override
    public String toString() {
        return "RPEither{" +
                "r1=" + r1 +
                ", r2=" + r2 +
                '}';
    }
}
