package com.haskforce.parsing.srcExtsDatatypes;

/**
 * ThenTrans    l (Exp l)
 */
public class ThenTrans extends QualStmtTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "ThenTrans{" +
                "exp=" + exp +
                '}';
    }
}
