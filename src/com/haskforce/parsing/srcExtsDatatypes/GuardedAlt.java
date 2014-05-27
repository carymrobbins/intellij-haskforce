package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * data GuardedAlt l = GuardedAlt l [Stmt l] (Exp l)
 */
public class GuardedAlt {
    public SrcInfoSpan srcInfoSpan;
    public StmtTopType[] stmts;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "GuardedAlt{" +
                "stmts=" + Arrays.toString(stmts) +
                ", exp=" + exp +
                '}';
    }
}
