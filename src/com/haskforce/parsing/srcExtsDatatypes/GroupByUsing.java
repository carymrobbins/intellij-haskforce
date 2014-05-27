package com.haskforce.parsing.srcExtsDatatypes;

/**
 * GroupByUsing l (Exp l) (Exp l)
 */
public class GroupByUsing extends QualStmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType e1;
    public ExpTopType e2;

    @Override
    public String toString() {
        return "GroupByUsing{" +
                "e1=" + e1 +
                ", e2=" + e2 +
                '}';
    }
}
