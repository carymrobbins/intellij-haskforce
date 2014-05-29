package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RPOp l (RPat l) (RPatOp l)
 */
public class RPOp extends RPatTopType {
    public SrcInfoSpan srcInfoSpan;
    public RPatTopType rpat;
    public RPatOpTopType rpatOp;

    @Override
    public String toString() {
        return "RPOp{" +
                "rpat=" + rpat +
                ", rpatOp=" + rpatOp +
                '}';
    }
}
