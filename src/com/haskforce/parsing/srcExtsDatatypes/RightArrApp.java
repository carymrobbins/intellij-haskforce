package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RightArrApp     l (Exp l) (Exp l)
 */
public class RightArrApp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "RightArrApp{" +
                "e1=" + e1 +
                ", e2=" + e2 +
                '}';
    }
}
