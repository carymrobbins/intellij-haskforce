package com.haskforce.parsing.srcExtsDatatypes;

import java.util.Arrays;

/**
 * GuardedRhs l [Stmt l] (Exp l)
 */
public class GuardedRhs {
    public SrcInfoSpan srcInfoSpan;
    public StmtTopType[] stmts;
    public ExpTopType exp;

    @Override
    public String toString() {
        return "GuardedRhs{" +
                "stmts=" + Arrays.toString(stmts) +
                ", exp=" + exp +
                '}';
    }
}
