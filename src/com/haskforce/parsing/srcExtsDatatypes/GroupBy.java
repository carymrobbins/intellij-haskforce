package com.haskforce.parsing.srcExtsDatatypes;

/**
 * GroupBy      l (Exp l)
 */
public class GroupBy extends QualStmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "GroupBy{" +
                "exp=" + exp +
                '}';
    }
}
