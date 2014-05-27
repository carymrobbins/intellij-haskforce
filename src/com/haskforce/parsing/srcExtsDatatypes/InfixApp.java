package com.haskforce.parsing.srcExtsDatatypes;

/**
 * InfixApp l (Exp l) (QOp l) (Exp l)
 */
public class InfixApp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public QOpTopType qop;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "InfixApp{" +
                "e1=" + e1 +
                ", qop=" + qop +
                ", e2=" + e2 +
                '}';
    }
}
