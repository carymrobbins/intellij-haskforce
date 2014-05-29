package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * ParComp  l (Exp l) [[QualStmt l]]
 */
public class ParComp extends ExpTopType {
    public SrcInfoSpan srcInfoSpan;
    public ExpTopType exp;
    public QualStmtTopType[][] qualStmts;

    @Override
    public String toString() {
        return "ParComp{" +
                "exp=" + exp +
                ", qualStmts=" + Arrays.toString(qualStmts) +
                '}';
    }
}
