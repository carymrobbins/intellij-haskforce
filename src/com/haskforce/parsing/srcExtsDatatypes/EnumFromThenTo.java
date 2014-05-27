package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  EnumFromThenTo l (Exp l) (Exp l) (Exp l)
 */
public class EnumFromThenTo extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType from;
    public ExpTopType step;
    public ExpTopType to;

    @Override
    public String toString() {
        return "EnumFromThenTo{" +
                "from=" + from +
                ", step=" + step +
                ", to=" + to +
                '}';
    }
}
