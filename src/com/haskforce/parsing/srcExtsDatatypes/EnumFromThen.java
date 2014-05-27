package com.haskforce.parsing.srcExtsDatatypes;

/**
 *  EnumFromThen l (Exp l) (Exp l)
 */
public class EnumFromThen extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType from;
    public ExpTopType step;

    @Override
    public String toString() {
        return "EnumFromThen{" +
                "from=" + from +
                ", step=" + step +
                '}';
    }
}
