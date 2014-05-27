package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ThenBy       l (Exp l) (Exp l)
 */
public class ThenBy extends QualStmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "ThenBy{" +
                "e1=" + e1 +
                ", e2=" + e2 +
                '}';
    }
}
