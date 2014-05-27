package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * ListComp l (Exp l) [QualStmt l]
 */
public class ListComp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;
    public QualStmtTopType[] qualStmts;

    @Override
    public String toString() {
        return "ListComp{" +
                "exp=" + exp +
                ", qualStmts=" + Arrays.toString(qualStmts) +
                '}';
    }
}
