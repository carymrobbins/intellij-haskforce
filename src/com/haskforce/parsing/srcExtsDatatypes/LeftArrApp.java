package com.haskforce.parsing.srcExtsDatatypes;

/**
 * LeftArrApp      l (Exp l) (Exp l)
 */
public class LeftArrApp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "LeftArrApp{" +
                "e1=" + e1 +
                ", e2=" + e2 +
                '}';
    }
}
