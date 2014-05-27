package com.haskforce.parsing.srcExtsDatatypes;

/**
 * GroupUsing   l (Exp l)
 */
public class GroupUsing extends QualStmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "GroupUsing{" +
                "exp=" + exp +
                '}';
    }
}
