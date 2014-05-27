package com.haskforce.parsing.srcExtsDatatypes;

/**
 * EnumFromTo l (Exp l) (Exp l)
 */
public class EnumFromTo extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType from;
    public ExpTopType to;

    @Override
    public String toString() {
        return "EnumFromTo{" +
                "from=" + from +
                ", to=" + to +
                '}';
    }
}
