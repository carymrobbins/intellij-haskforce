package com.haskforce.parsing.srcExtsDatatypes;

/**
 * data IfAlt l = IfAlt l (Exp l) (Exp l)
 */
public class IfAlt {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "IfAlt{" +
                "e1=" + e1 +
                ", e2=" + e2 +
                '}';
    }
}
