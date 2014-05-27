package com.haskforce.parsing.srcExtsDatatypes;

/**
 * RightArrHighApp l (Exp l) (Exp l)
 */
public class RightArrHighApp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "RightArrHighApp{" +
                "e1=" + e1 +
                ", e2=" + e2 +
                '}';
    }
}
