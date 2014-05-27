package com.haskforce.parsing.srcExtsDatatypes;

/**
 * LeftArrHighApp  l (Exp l) (Exp l)
 */
public class LeftArrHighApp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "LeftArrHighApp{" +
                "e1=" + e1 +
                ", e2=" + e2 +
                '}';
    }
}
